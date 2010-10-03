{-# LANGUAGE GeneralizedNewtypeDeriving, GADTs, ExistentialQuantification, MultiParamTypeClasses, RankNTypes #-}
module AionBot ( AionBot
               , runAionBot
               , runAbortableAionBot
               , parkMouse
               , getPlayer
               , getPlayerEntity
               , getTarget
               , getEntities
               , getTargetting
               , getTargettingMe

               , StrafeDirection (..)

               , aimTarget
               , nextTarget
               , rotateCamera

               , strafe
               , walk
               , backpedal
               , jump
               , walkToTarget
                 
               , killTarget
               , kill

               , grind

               ) where

import Common
import Data.Binary ( encode )
import Data.Map (Map)
import Data.Maybe
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
import AionBotConfig

newtype AionBot a = AionBot { unBot :: MicroThreadT (StateT BotState IO) a }
    deriving ( Functor, Monad, MonadMicroThread )

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
click btn =
    getChannel >>= \c ->
        do liftIO $ sendCommand c (ChangeMouseState Down btn)
           delay 0.1
           liftIO $ sendCommand c (ChangeMouseState Up btn)
           delay 0.1

mouseTo x y =
    getChannel >>= \c ->
        do liftIO $ sendCommand c $ AbsMoveMousePerc x y
           delay 0.1

keyState state code =
    getChannel >>= \c ->
        do liftIO $ sendCommand c $ ChangeKeyState state code
           delay 0.1

keyPress code =
    getChannel >>= \c ->
        do liftIO $ sendCommand c (ChangeKeyState Down code) 
           delay 0.1
           liftIO $ sendCommand c (ChangeKeyState Up code)
           delay 0.1

---
--- FIXME: how to do this in core microthread monad?
---
timeoutWithRVal :: Float -> AionBot a -> AionBot (Maybe a)
timeoutWithRVal max_t action =
    do mvar <- liftIO $ newEmptyMVar
       timeout max_t $
               do r <- action
                  liftIO $ putMVar mvar r
       liftIO . tryTakeMVar $ mvar

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

-- walk backwards
backpedal :: AionBot ()
backpedal =
    do keyState Down keyBackward
       finally (keyState Up keyBackward) $
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

-- pull current target
pullTarget :: AionBot ()
pullTarget = getTarget >>= pull
    where
      pull Nothing  = return ()
      pull (Just t) =
          do info $ "pulling " ++ (entity_name t)
             killTarget

-- attack current target
attackTarget :: AionBot ()
attackTarget =
    getTarget >>= attack
  where
    attack Nothing  = return ()
    attack (Just t) = do info $ "attacking " ++ (entity_name t)
                         keyPress keyAttack

-- kill current target
killTarget :: AionBot ()
killTarget =
    getTarget >>= kill
    where
      kill Nothing  = return ()
      kill (Just t) =
          do attackTarget
             withSpark rotate_skills $ \_ ->
                 do wait (isDead . entity_id $ t)
                    info $ "victory, " ++ entity_name t ++ " DIED!"
      rotate_skills =
          do c <- getConfig
             info $ "executing combat rotation!"
             execute (combat_rotation c)

-- select & kill entity
kill :: Entity -> AionBot ()
kill t = do s <- select t
            when s $ killTarget

-- select entity, max few secs to try to select it
select :: Entity -> AionBot Bool
select e = do
    r <- timeoutWithRVal 3.0 $ do
             withSpark aim $ \_ ->
                 try_select
    case r of
      Just True -> return True
      _         -> do info $ "failed to select " ++ entity_name e
                      return False

    where
      aim = aimEntity e >> aim
      try_select :: AionBot Bool
      try_select =
          do t <- getTarget
             case t of
               Just t | t == e    -> return True
               _                  -> nextTarget >> try_select
    
------------------------------
---- PRIMARY GRINDING ROUTINE! awesome in its simplicity
------------------------------
grind :: AionBot ()
grind =
    invariant alive died $
              grindy_grind
    where
      grindy_grind =
          do invariant (not <$> inCombat) (retaliate >> grind) pickGrindTarget
             pullTarget
             grindy_grind

      alive = not <$> isPlayerDead

pickGrindTarget :: AionBot ()
pickGrindTarget =
    do picked <- timeoutWithRVal 4 $ pickStatic
       case picked of
         Just True -> return ()
         _         -> rotateCamera 60 >> pickGrindTarget
    where
      pickStatic :: AionBot Bool
      pickStatic = do
        t <- getTarget
        case t of
          Nothing -> nextTarget >> pickStatic
          Just t  ->
              do ok <- suitableGrindTarget t
                 if ok
                   then return True
                   else nextTarget >> pickStatic

suitableGrindTarget :: Entity -> AionBot Bool
suitableGrindTarget t =
    do ply <- getPlayer
       cfg <- getConfig
       its_target <- getEntity (entity_target_id t)
       let its_target_type = maybe EOther entity_type its_target

       case () of
         _ | dead                       -> return False -- don't kill corpses
           | not (level_ok ply cfg)     -> return False -- don't kill out of level range
           | its_target_type == EPlayer -> return False -- don't kill if targetting other player
           | otherwise                  -> return True
    where
      dead = isDeadPure t
      targetting id = entity_target_id t == id
      level_ok ply cfg =
          level >= ply_level - (threshold_grind_lower_level cfg) &&
          level <= ply_level + (threshold_grind_upper_level cfg)
          where level     = entity_level t
                ply_level = player_level ply

died :: AionBot ()
died = 
    do info "I AM DEAD :("
       return () -- TODO: resurrect


-- we were happily doing some interesting stuff when bad things attacked us
retaliate :: AionBot ()
retaliate =
    do info "ROMA VICTA!!!! (retaliating against surprise attack)"
       ply  <- getPlayer
       mobs <- distanceSort (player_pos ply) <$> getCombatants
       case mobs of
         [ ]   -> info "nobody to retaliate on :(" >> return ()
         (m:_) -> whack m
    where
      whack m = do info $ (entity_name m) ++ "'s blood will fill a river"
                   kill m
                   retaliate

distanceSort :: Vec3 -> [Entity] -> [Entity]
distanceSort p es = sortBy (comparing distance) es
    where
      distance e = let p' = entity_pos e in
                   len (p' $- p)
----
---- Bot state
----
data BotState = BotState { agent_channel   :: CommandChannel IO
                         , camera          :: Camera
                         , player          :: Player
                         , entities        :: [Entity]
                         , entity_map      :: Map Int Entity
                         , combat_map      :: [CombatMob]
                         , config          :: AionBotConfig
                         }

-- state of combat with given mob
data CombatMob = CombatMob { combat_id :: EntityID
                           , combat_last_time :: Float }

-- periodically read state from GameState object
stateReader :: GameState -> Float -> AionBot ()
stateReader gs period =
    updateState gs >> delay period >> stateReader gs period

-- periodically expire mobs from combat map
combatExpirator :: Float -> AionBot ()
combatExpirator period =
    do s <- liftState get
       combat' <- foldM expire [] (combat_map s)
       liftState . put $ s { combat_map = combat' }
       delay period
       debug $ "in combat with " ++ show (length combat') ++ " monsters."
       combatExpirator period
    where
      expire :: [CombatMob] -> CombatMob -> AionBot [CombatMob]
      expire acc m@(CombatMob id target_t0) =
          do e <- getEntity id
             let dead = case e of
                          Nothing -> False -- let it expire by timer
                          Just e  -> isDeadPure e
             t <- time
             if dead || t - target_t0 > 8
                then return acc
                else return (m:acc)

-- periodically insert/refresh mobs to combat map
combatInserter :: Float -> AionBot ()
combatInserter period =
    do es <- getEntities
       p  <- getPlayer
       mapM_ (update p) es
       delay period
       combatInserter period
    where
      update ply e
          | player_id ply == entity_id e        = return ()
          | player_id ply == entity_target_id e = combatMark e
          | otherwise                           = return ()

-- set as being in combat with entity
combatMark :: Entity -> AionBot ()
combatMark e =
    do s <- liftState get
       t <- time
       let combat  = filter (\mob -> combat_id mob /= entity_id e) (combat_map s)
           combat' = CombatMob (entity_id e) t : combat
       liftState . put $ s { combat_map = combat' }

-- check if player is in combat with xxx
inCombatWith :: Entity -> AionBot Bool
inCombatWith e =
    do combatants <- getCombatants
       return $ e `elem` combatants

-- check if player is in combat
inCombat :: AionBot Bool
inCombat =
    do combatants <- getCombatants
       return . not . null $ combatants

isDeadPure :: Entity -> Bool
isDeadPure e | entity_hp e <= 0 || entity_state e == EntityDead = True
             | otherwise = False

isDead :: EntityID -> AionBot Bool
isDead id =
    getEntity id >>= test
    where
      test Nothing  = return False
      test (Just e) = return $ isDeadPure e

isPlayerDead :: AionBot Bool
isPlayerDead =
    do p <- getPlayer
       isDead (player_id p)
       
isFullHealth :: EntityID -> AionBot Bool
isFullHealth id =
    getEntity id >>= test
    where
      test Nothing  = return False
      test (Just e) = return (entity_hp e == 100)

execute :: Rotation -> AionBot ()
execute r@(Repeat elems) = mapM_ execute_elem elems >> execute r
execute r@(Once elems)   = mapM_ execute_elem elems

execute_elem :: RotationElem -> AionBot ()
execute_elem e = debug (show e) >> execute_elem' e
execute_elem' :: RotationElem -> AionBot ()
execute_elem' (Rotation nest) = execute nest
execute_elem' (KeyPress key) = keyPress key
execute_elem' (KeyHold key) = keyState Down key
execute_elem' (KeyRelease key) = keyState Up key
execute_elem' (Delay dt) = delay dt

-- access list of mobs in combat with player
getCombatants :: AionBot [Entity]
getCombatants =
    do s <- liftState get
       let combat = map combat_id $ combat_map s
       entities <- mapM getEntity combat
       return $ catMaybes entities

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
getEntity :: EntityID -> AionBot (Maybe Entity)
getEntity id = liftState get >>= \s -> return $ M.lookup id (entity_map s)

-- access current entity list
getEntities :: AionBot [Entity]
getEntities = liftState get >>= return . entities

-- access current player target. don't return dead mobs
getTarget :: AionBot (Maybe Entity)
getTarget =
    do p <- getPlayerEntity
       e <- getEntity (entity_target_id p)
       case e of
         Just e | not (isDeadPure e),
                  (entity_id e /= 0) -> return $ Just e
         _ -> return Nothing

-- all targetting given entity
getTargetting :: EntityID -> AionBot [Entity]
getTargetting id =
    do es <- getEntities
       return $ filter (\e' ->    entity_target_id e' == id
                               && not (isDeadPure e')
                       ) es

-- all targetting player
getTargettingMe :: AionBot [Entity]
getTargettingMe = getPlayer >>= \p -> getTargetting (player_id p)

-- access bot configuration
getConfig :: AionBot AionBotConfig
getConfig = liftState get >>= return . config

-- fetch complete state from game
updateState :: GameState -> AionBot ()
updateState gs =
    do cam <- liftIO $ readIORef (game_camera gs)
       ply <- liftIO $ readIORef (game_player gs)
       ent <- filter interesting <$> (liftIO $ readIORef (game_entities gs))
       let ent_m = M.fromList $ zip (map entity_id ent) ent
       liftState . modify $ \s -> s { camera = cam
                                    , player = ply
                                    , entities = ent
                                    , entity_map = ent_m }
    where
      interesting e | entity_type e == ENPC = True
                    | entity_type e == EPlayer = True
                    | otherwise = False

runAionBot :: CommandChannel IO -> GameState -> AionBot () -> IO ()
runAionBot agent_ch game_state aionbot =
    do t0 <- getCurrentTime
       let mt = unBot $
                do -- initial state fetch into monad
                   updateState game_state
                   -- periodic state updates, combat updates etc
                   withSpark (stateReader game_state 0.01) $ \_ ->
                       withSpark (combatExpirator 0.5) $ \_ ->
                           withSpark (combatInserter 0.5) $ \_ ->
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
                  , entity_map = undefined
                  , combat_map = []
                  , config = defaultConfig }

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
                  do debug "ABORTING!"
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

info :: String -> AionBot ()
info s = liftIO (putStrLn s)

debug :: String -> AionBot ()
debug s = liftIO (debugIO s)