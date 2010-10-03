{-# LANGUAGE GeneralizedNewtypeDeriving, GADTs, ExistentialQuantification, MultiParamTypeClasses, RankNTypes #-}
module AionBot ( AionBot, runAionBot
               , updateState
               , updateStateFromChannel
               , stateReader

               , parkMouse

               , getPlayer
               , getPlayerEntity
               , getTarget
               , getEntities
               , aimTarget
               , nextTarget
               , walkToTarget
               , rotateCamera
               ) where

import Common
import Data.Binary ( encode )
import Data.Map (Map)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Trans
import System.Time
import System.IO
import Text.Printf
import Network.Socket hiding (sendTo, recvFrom)
import Network.Socket.ByteString (sendTo, recvFrom)

import GameState
import MicroThread
import Aion hiding (getCamera)
import Channel ( Channel )
import qualified Channel as C
import Math
import Keys

import Packet

newtype AionBot a = AionBot { unBot :: MicroThreadT (StateT BotState IO) a }
    deriving ( Monad, MonadMicroThread )

instance MonadIO AionBot where
    liftIO = AionBot . lift . lift

liftState = AionBot . lift

getChannel :: AionBot (Channel IO)
getChannel =
    do s <- liftState get
       return $ channel s


----
---- Wrappers around some comm channel functions so we don't have to pass channel everywhere
----
setMousePos x y = getChannel >>= \c -> liftIO $ C.setMousePos c x y
sendKey state code = getChannel >>= \c -> liftIO $ C.sendKey c state code
sendKeyPress code = getChannel >>= \c -> liftIO $ C.sendKeyPress c code
sendMouseBtn state btn = getChannel >>= \c -> liftIO $ C.sendMouseBtn c state btn
sendMouseClick btn = getChannel >>= \c -> liftIO $ C.sendMouseClick c btn
sendMouseMove dx dy = getChannel >>= \c -> liftIO $ C.sendMouseMove c dx dy

-- udp send mouse move
sendMouseMoveFast dx dy =
    do s <- liftState get
       let udps = udpsock s
           addr = udpaddr s
           p = Packet { packet_id = 0
                      , packet_addr = Nothing
                      , packet_data = MoveMouse (fromIntegral dx) (fromIntegral dy) }
       liftIO $ sendTo udps (B.concat . BL.toChunks $ encode p) addr

----
---- Control Commands
----

-- place mouse in safe spot for performing clicks (no mobs, no ui elements)
parkMouse :: AionBot ()
parkMouse = centerMouse -- setMousePos 0.33 0.1 >> delay 0.1

centerMouse :: AionBot ()
centerMouse = setMousePos 50 50 >> delay 0.1

-- camera rotation value best matching a direction
rotationOfDir :: Vec3 -> Float
rotationOfDir (Vec3 x y z) =
    let rad = fst $ minimumBy (comparing snd) (zip angles diffs)
        deg = rad2deg rad in
    snap (180-deg) -- some weird aioness
    where
      angles  = map deg2rad [ 0..360 ]
      diffs   = map diff angles
      diff a  = let x' = cos a
                    y' = sin a
                in abs(x-x') + abs(y-y')
      deg2rad a = a / 360 * 2 * pi
      rad2deg a = a * 360 / 2 / pi

-- angle snapping to -180 .. 180 range
snap :: Float -> Float
snap a | a >= 180    = snap (a-360)
       | a <  (-180) = snap (a+360)
       | otherwise   = a

-- rotate camera by given amount
rotateCamera :: Float -> AionBot ()
rotateCamera delta =
    -- shouldn't be taking more than few secs
    do timeout 2.0 $
               do parkMouse
                  sendMouseBtn C.Down C.R
                  finally (sendMouseBtn C.Up C.R) $ do
                    delay 0.15
                    c <- getCamera
                    -- target rotation angle
                    let t_r = snap $ camera_rot c + delta
                    rotate (camera_rot c) t_r
       delay 0.1
       return ()
    where
      epsilon = 2
      rotate r0 t_r
          | abs (t_r - r0) < epsilon = return () -- hooray rotated
          | otherwise =
              do let diff_a = max t_r r0 - min t_r r0
                     diff_b = 360 - diff_a
                     d =
                         case () of
                           _ | r0 >= t_r, diff_a <= diff_b -> -diff_a
                             | r0 >= t_r, diff_a >  diff_b ->  diff_b
                             | r0 <  t_r, diff_a <= diff_b ->  diff_a
                             | otherwise                   -> -diff_b
                     speed = min 20 . max 2 $ (abs d / 5) ** 1.5 -- nice smooth function for rotation speed; lower when nearing target rotation
                     dx = speed * (- (signum d))
                 -- debug $ printf "tr=%f r=%f dx=%f d=%f" t_r r0 dx d
                 sendMouseMoveFast (round dx) 0
                 -- delay and fetch new camera orientation from game, then continue with rotation
                 delay 0.1
                 c <- getCamera
                 rotate (camera_rot c) t_r
    
-- aim given direction
aimDirection :: Vec3 -> AionBot ()
aimDirection d =
    do p <- getPlayer
       let r = rotationOfDir d
           r_diff = r - player_rot p
       rotateCamera r_diff

-- aim given point
aimPoint :: Vec3 -> AionBot ()
aimPoint p =
    do ply <- getPlayer
       let dir = norm $ (p $- player_pos ply)
       aimDirection dir

-- aim specified entity
aimEntity :: Entity -> AionBot ()
aimEntity e = aimPoint (entity_pos e)

-- aim current target
aimTarget :: AionBot ()
aimTarget =
    do t <- getTarget
       case t of
         Nothing -> return ()
         Just t  -> aimEntity t

-- select next target, give up if can't within small timeout
nextTarget :: AionBot ()
nextTarget =
    do t <- getTarget
       sendKeyPress keyNextTarget
       timeout 2 (next t)
       return ()
  where
    next t =
        do t' <- getTarget
           case () of
             _ | t == t'   -> delay 0.25 >> next t
               | otherwise -> return ()

finishWalkThreshold :: Float
finishWalkThreshold = 10

-- walk up to given point
walkTo :: Float -> Vec3 -> AionBot ()
walkTo maxtime p =
    finished >>= \f ->
        case f of
          True  -> return ()
          False -> do aimPoint p
                      withSpark keepAim $ \_ ->
                          do sendKey C.Down keyForward
                             timeout maxtime walk
                             sendKey C.Up keyForward
  where
    walk = finished >>= \f ->
           case f of
             True  -> sendKey C.Up keyForward >> debug "TARGET REACHED"
             False -> delay 0.25 >> walk
    keepAim = aimPoint p >> keepAim
    finished = getPlayer >>= \ply ->
               let ply_pos = player_pos ply
                   dist = len $ ply_pos $- p in
               return $ dist <= finishWalkThreshold

-- walk up to target
walkToTarget :: Float -> AionBot ()
walkToTarget maxtime =
    getTarget >>= walk
    where walk Nothing = return ()
          walk (Just t) = walkTo maxtime (entity_pos t)

----
---- Bot state
----
data BotState = BotState { channel   :: Channel IO
                         , udpsock   :: Socket
                         , udpaddr   :: SockAddr
                         , camera    :: Camera
                         , player    :: Player
                         , entities  :: [Entity]
                         , entity_map :: Map Int Entity }

-- fetch complete state from game
updateStateFromChannel :: AionBot ()
updateStateFromChannel =
    do c <- getChannel
       p <- liftIO $ C.recvPlayer c
       e <- liftIO $ C.recvEntityList c
       let e_map = M.fromList $ zip (map entity_id e) e
       liftState . modify $ \s -> s { player = p, entities = e, entity_map = e_map }

-- fetch complete state from game
updateState :: GameState -> AionBot ()
updateState gs =
    do cam <- liftIO $ readIORef (game_camera gs)
       ply <- liftIO $ readIORef (game_player gs)
       ent <- liftIO $ readIORef (game_entities gs)
       entm <- liftIO $ readIORef (game_entity_map gs)
       liftState . modify $ \s -> s { camera = cam
                                    , player = ply
                                    , entities = ent
                                    , entity_map = entm }

-- access camera
getCamera :: AionBot Camera
getCamera = liftState get >>= return . camera

-- access player
getPlayer :: AionBot Player
getPlayer = liftState get >>= return . player

-- access player as entity
getPlayerEntity :: AionBot Entity
getPlayerEntity = getPlayer >>= \p ->
                  getEntity (player_id p) >>= \e ->
                  case e of
                    Nothing -> error "There is no player entity!"
                    Just e -> return e

-- access entity by ID
getEntity :: Int -> AionBot (Maybe Entity)
getEntity id = liftState get >>= \s -> return $ M.lookup id (entity_map s)

-- access current entity list
getEntities :: AionBot [Entity]
getEntities = liftState get >>= return . entities

-- access current player target
getTarget :: AionBot (Maybe Entity)
getTarget = getPlayerEntity >>= \p -> getEntity (entity_target_id p)

--theBot :: AionBot ()
--theBot = spark (stateReader 0.5) >> idle

idle :: AionBot ()
idle =
    do delay 0.25
       liftIO $ putStrLn "ha, ha" >> hFlush stdout
       idle

-- periodically read state from GameState object
stateReader :: GameState -> Float -> AionBot ()
stateReader gs period =
    updateState gs >> delay period >> stateReader gs period

runAionBot :: Channel IO -> AionBot () -> IO ()
runAionBot c aionbot =
    do udp <- socket AF_INET Datagram defaultProtocol
       a <- inet_addr "192.168.1.2"
       let addr = SockAddrInet 5555 a
       t0 <- getClockTime
       let mt = unBot aionbot
           state = runMicroThreadT (handler t0) mt
       evalStateT state (s0 udp addr)
  where
    s0 udp addr =
         BotState { channel = c
                  , camera = undefined
                  , player = undefined
                  , entities = undefined
                  , entity_map = undefined
                  , udpsock = udp
                  , udpaddr = addr }

    handler :: ClockTime -> Request a -> StateT BotState IO a
    handler t0 (ThreadDelay secs) = liftIO . threadDelay $ round (secs * 10^6)
    handler t0 GetCurrentTime = liftIO $ diffTime t0
    --handler t0 (Trace msg) = return ()
    handler t0 (Trace msg) = liftIO . putStrLn $ "thread> " ++ msg

    diffTime :: ClockTime -> IO Float
    diffTime t0 =
        do t1 <- getClockTime
           let diff = diffClockTimes t1 t0
           return $     (fromIntegral (tdHour diff) * 60 * 24)
                      + (fromIntegral (tdMin diff) * 60)
                      + (fromIntegral $ tdSec diff)
                      + (fromIntegral (tdPicosec diff) / 10^12)

--runTheBot :: Channel IO -> IO ()
--runTheBot channel = runAionBot channel theBot

debug :: String -> AionBot ()
debug s = liftIO (putStrLn s)

