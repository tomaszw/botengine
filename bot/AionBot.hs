{-# LANGUAGE GeneralizedNewtypeDeriving, GADTs, ExistentialQuantification, MultiParamTypeClasses, RankNTypes #-}
module AionBot ( AionBot
               , runAionBot
               , runAbortableAionBot
               , parkMouse
               , getPlayer
               , getPlayerEntity
               , getTarget
               , getEntities
               , aimTarget
               , nextTarget
               , StrafeDirection (..)
               , strafe
               , walk
               , jump
               , walkToTarget
               , rotateCamera
               ) where

import Common
import Data.Binary ( encode )
import Data.Map (Map)
import Data.Time
import Data.Time.Clock
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import qualified Data.Map as M
import Control.Monad.State

import Text.Printf
import Network.Socket hiding (sendTo, recvFrom)
import Network.Socket.ByteString (sendTo, recvFrom)

import MicroThread
import Aion hiding (getCamera)
import RemoteCommand
import GameState
import Math
import Keys

newtype AionBot a = AionBot { unBot :: MicroThreadT (StateT BotState IO) a }
    deriving ( Monad, MonadMicroThread )

data StrafeDirection = StrafeLeft | StrafeRight

instance MonadIO AionBot where
    liftIO = AionBot . lift . lift

liftState = AionBot . lift

getChannel :: AionBot (CommandChannel IO)
getChannel =
    do s <- liftState get
       return $ agent_channel s


----
---- Wrappers around some comm channel functions so we don't have to pass channel everywhere
----
click btn = getChannel >>= \c -> liftIO $ sendCommand c (ChangeMouseState Down btn)>> sendCommand c (ChangeMouseState Up btn)
mouseTo x y = getChannel >>= \c -> liftIO $ sendCommand c $ AbsMoveMousePerc x y
keyState state code = getChannel >>= \c -> liftIO $ sendCommand c $ ChangeKeyState state code
keyPress code = getChannel >>= \c -> liftIO $ sendCommand c (ChangeKeyState Down code) >> sendCommand c (ChangeKeyState Up code)

----
---- Control Commands
----

-- place mouse in safe spot for performing clicks (no mobs, no ui elements)
parkMouse :: AionBot ()
parkMouse = centerMouse -- mouseTo 0.33 0.1 >> delay 0.1

centerMouse :: AionBot ()
centerMouse = mouseTo 50 50 >> delay 0.1

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

waitToRotateCamera :: Float -> AionBot()
waitToRotateCamera t_r =
    wait rotated
    where
      rotated = 
          do c <- getCamera
             let diff = abs (t_r - camera_rot c)
             return $ diff <= 2

-- rotate camera by given amount
rotateCamera :: Float -> AionBot ()
rotateCamera delta =
    -- shouldn't be taking more than few secs
    do c <- getCamera
       let t_r = snap $ camera_rot c + delta
       ch <- getChannel
       liftIO $ sendCommand ch (OrientCamera t_r)
       -- wait until rotation matches
       timeout 2.0 (waitToRotateCamera t_r)
    
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
       keyPress keyNextTarget
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

--
strafe :: StrafeDirection -> AionBot()
strafe d =
    do keyState Down key
       finally (keyState Up key) $
               waitForever
    where
      key = case d of
              StrafeLeft  -> keyStrafeLeft
              StrafeRight -> keyStrafeRight
              
--
jump :: AionBot ()
jump = keyPress keyJump

--

-- walk forward
walk :: AionBot ()
walk =
    do keyState Down keyForward
       finally (keyState Up keyForward) $
               waitForever

-- walk up to given point
walkTo :: Float -> Vec3 -> AionBot ()
walkTo maxtime p =
    finished >>= \f ->
        case f of
          True  -> return ()
          False -> do aimPoint p
                      withSpark keepAim $ \_ ->
                          do keyState Down keyForward
                             timeout maxtime walk
                             keyState Up keyForward
  where
    walk = finished >>= \f ->
           case f of
             True  -> keyState Up keyForward >> info "TARGET REACHED!"
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
data BotState = BotState { agent_channel   :: CommandChannel IO
                         , camera    :: Camera
                         , player    :: Player
                         , entities  :: [Entity]
                         , entity_map :: Map Int Entity }

-- periodically read state from GameState object
stateReader :: GameState -> Float -> AionBot ()
stateReader gs period =
    updateState gs >> delay period >> stateReader gs period

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

-- fetch complete state from game
updateState :: GameState -> AionBot ()
updateState gs =
    do cam <- liftIO $ readIORef (game_camera gs)
       ply <- liftIO $ readIORef (game_player gs)
       ent <- liftIO $ readIORef (game_entities gs)
       let ent_m = M.fromList $ zip (map entity_id ent) ent
       liftState . modify $ \s -> s { camera = cam
                                    , player = ply
                                    , entities = ent
                                    , entity_map = ent_m }

runAionBot :: CommandChannel IO -> GameState -> AionBot () -> IO ()
runAionBot agent_ch game_state aionbot =
    do t0 <- getCurrentTime
       let mt = unBot $
                do -- initial state fetch into monad
                   updateState game_state
                   -- periodic state updates
                   withSpark (stateReader game_state 0.01) $ \_ ->
                             -- rest of botting
                             aionbot
           state = runMicroThreadT (ioHandler t0) mt
       evalStateT state s0
  where
    s0 =
         BotState { agent_channel = agent_ch
                  , camera = undefined
                  , player = undefined
                  , entities = undefined
                  , entity_map = undefined }

-- forks it in background
runAbortableAionBot :: CommandChannel IO -> GameState -> AionBot () -> IO (ThreadId, IO (), MVar ())
runAbortableAionBot agent_ch game_state action =
    do abort_var <- newEmptyMVar
       abort_fun_var <- newEmptyMVar
       finished_var <- newEmptyMVar
       id <- forkIO $ do
               runAionBot agent_ch game_state $ do
                        withSpark (aborter abort_var) $ \_ ->
                                  action
               putMVar finished_var ()
       return $ (id, abort_fun abort_var, finished_var)
    where
      aborter var =
          do empty <- liftIO $ isEmptyMVar var
             when (not empty) $
                  abort
             delay 0.1
             aborter var
      abort_fun var =
          do tryPutMVar var ()
             return ()

ioHandler :: (MonadIO m) => UTCTime -> Request a -> m a
ioHandler t0 (ThreadDelay secs) = liftIO . threadDelay $ round (secs * 10^6)
ioHandler t0 GetCurrentTime = liftIO $ ioDiffTime t0
--ioHandler t0 (Trace msg) = liftIO . putStrLn $ "thread> " ++ msg
ioHandler t0 (Trace msg) = return ()

ioDiffTime :: UTCTime -> IO Float
ioDiffTime t0 =
    do t1 <- getCurrentTime
       return . realToFrac $ diffUTCTime t1 t0

debug :: String -> AionBot ()
debug s = liftIO (putStrLn s)

info :: String -> AionBot ()
info s = liftIO (putStrLn s)
