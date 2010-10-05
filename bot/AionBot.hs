{-# LANGUAGE GeneralizedNewtypeDeriving, GADTs, ExistentialQuantification, MultiParamTypeClasses, RankNTypes, BangPatterns #-}
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
               , loot

               , grind
               , pickGrindTarget
               , healSelf
               ) where

import Common
import Data.Binary ( encode )
import Data.Map (Map)
import Data.Maybe

import System.Random
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

-- is there a better way
newtype AionBot_ s a = AionBot_ { unBot :: MicroThreadT s (StateT BotState IO) a }
    deriving ( Functor, Monad, MonadMicroThread )

type AionBot a = forall s. AionBot_ s a

instance MonadIO (AionBot_ s) where
    liftIO = AionBot_ . lift . lift

liftState :: StateT BotState IO a -> AionBot_ s a
liftState = AionBot_ . lift

-------
data StrafeDirection = StrafeLeft | StrafeRight

getChannel :: AionBot (CommandChannel IO)
getChannel =
    do s <- liftState get
       return $ agent_channel s

sendCommand' ch cmd = debug (show cmd) >> liftIO (sendCommand ch cmd)

----
---- Wrappers around some comm channel functions so we don't have to pass channel everywhere
----
click btn =
    getChannel >>= \c ->
        finally (sendCommand' c (ChangeMouseState Up btn)) $
                do sendCommand' c (ChangeMouseState Down btn)
                   delay 0.05

mouseTo x y =
    getChannel >>= \c ->
        do sendCommand' c $ AbsMoveMousePerc x y
           delay 0.01

keyState state code =
    getChannel >>= \c ->
        do sendCommand' c $ ChangeKeyState state code
           delay 0.05

keyPress code =
    getChannel >>= \c ->
        finally (sendCommand' c (ChangeKeyState Up code)) $
                do sendCommand' c (ChangeKeyState Down code) 
                   delay 0.05

----
---- Control Commands
----

-- place mouse in safe spot for performing clicks (no mobs, no ui elements)
parkMouse :: AionBot ()
parkMouse = mouseTo 0.33 0.1 >> delay 0.1 -- centerMouse -- mouseTo 0.33 0.1 >> delay 0.1

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
       sendCommand' ch (OrientCamera t_r)
       -- wait until rotation matches
       timeout 2.0 (waitToRotateCamera t_r)
       return ()
    
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
                             finally ( keyState Up keyForward ) $
                                     timeout maxtime walk
                             return ()
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

-- pull current target. false if we didn't, or pulled wrong target
pullTarget :: AionBot Bool
pullTarget = getTarget >>= pull where
    pull Nothing  = return False
    pull (Just t) =
        do info $ "pulling " ++ (entity_name t)
           attackTarget
           timeout 15 $
                   noStuck attackTarget $
                           wait inCombat
           c <- getCombatants
           case () of
             _ | t `elem` c -> return True  -- we managed to pull the correct target
               | otherwise  -> return False -- uh we're in combat with someone else

-- attack current target
attackTarget :: AionBot ()
attackTarget =
    getTarget >>= attack
  where
    attack Nothing  = return ()
    attack (Just t) = do info $ "attacking " ++ (entity_name t)
                         keyPress keyAttack
                         keyPress keyAttack
                         keyPress keyAttack
                         keyPress keyAttack
                         keyPress keyAttack

-- kill current target
killTarget :: AionBot ()
killTarget =
    getTarget >>= \t -> kill t -- (withSpark aim $ \_ -> kill t)
    where
      aim = aimTarget >> delay 2 >> aim
      kill Nothing  = return ()
      kill (Just t) =
          do won <- hold (amTargetting t) $ do
               attackTarget
               noStuck attackTarget $ wait inCombat
               withSpark rotate_skills $ \_ ->
                   do wait (isDead . entity_id $ t)
                      info $ "victory, " ++ entity_name t ++ " DIED!"
                      return True
             when (won == Just True) $ do
               c <- inCombat
               when ( not c ) $
                  loot
             return ()

      rotate_skills =
          do c <- getConfig
             info $ "executing combat rotation!"
             execute (combat_rotation c)

-- select & kill entity
kill :: Entity -> AionBot ()
kill t = do s <- select t
            when s $ killTarget

amTargetting :: Entity -> AionBot Bool
amTargetting e = getTarget >>= \t -> return (t == Just e)

--
loot :: AionBot ()
loot = delay 0.5 >> info "LOOTING!" >> getConfig >>= \c -> keyPress (loot_key c)

-- select entity, max few secs to try to select it
select :: Entity -> AionBot Bool
select e = do
    r <- timeout 3.0 $ do
             withSpark aim $ \_ ->
                 try_select
    case r of
      Just True -> do info $ "SELECTED " ++ entity_name e
                      return True
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
    do info "HAPPY GRIND!"
       r <- hold alive $ grindy_grind
       case r of
         Nothing -> died >> grind
         Just () -> grind
    where
      grindy_grind =
          do info "grindy grind"
             saf <- hold safe $
                  do hp <- getPlayerHealthPercent
                     when (hp < 80) (delay 1 >> healSelf)
                     pickGrindTarget
             case saf of
               Nothing -> retaliate >> delay 0.25 -- we got attacked
               Just () ->
                   do pulled <- pullTarget
                      case () of
                        _ | pulled    -> killTarget
                          | otherwise -> info "ABORTED PULL"
             grindy_grind

      alive = not <$> isPlayerDead
      pull_check = do
          -- just make sure noone else is attacking us
          maybe_t <- getTarget
          c <- getCombatants
          case () of
            _ | Just t <- maybe_t, not (null c), not (t `elem` c) -> return False -- stop pulling, we are attacked by elsewhere
              | Just t <- maybe_t, t `elem` c                     -> return True
              | otherwise                                         -> delay 0.1 >> pull_check


safe :: AionBot Bool
safe =
    do c   <- inCombat
       t   <- timeSinceCombat
       cfg <- getConfig
       case () of
         _ | c == False && t >= safe_margin cfg -> return True
           | otherwise -> return False

pickGrindTarget :: AionBot ()
pickGrindTarget =
    do debug "LOOKING FOR GRIND TARGET!"
       picked <- timeout 3 $ pickStatic
       case picked of
         Just True -> debug "PICKED grind target!"
         _         -> debug "ROTATE, repeat" >> spark (rotateCamera 60) >> pickGrindTarget
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
    do combat <- inCombat
       when combat $ do
         info $ printf "ROMA VICTA!!!! (retaliating against surprise attack)"
         t <- timeout 3 pickAggressor
         case t of
           Nothing -> info "FAIL to select an aggressor" >> aimAggressor
           Just t  -> whack t

    where
      pickAggressor =
          do t <- getTarget
             c <- getCombatants
             case () of
               _ | Just t <- t, t `elem` c -> return t
                 | otherwise               -> nextTarget >> pickAggressor
      aimAggressor =
          do c <- getCombatants
             p <- getPlayer
             case distanceSort (player_pos p) c of
               (m : _) -> aimEntity m
               _ -> return ()

      whack m =
          do info $ (entity_name m) ++ "'s blood will fill a river"
             kill m

healSelf :: AionBot ()
healSelf =
    do info "Healing myself!" 
       cfg <- getConfig
       execute (heal_self_rotation cfg)
       wait ( getPlayerHealthPercent >>= \p -> return $ p == 100 )
       info "HEALTH FULL"
       -- standup
       keyPress ( keyQuickbarBase + 9 )
       delay 1

distanceSort :: Vec3 -> [Entity] -> [Entity]
distanceSort p es = sortBy (comparing distance) es
    where
      distance e = let p' = entity_pos e in
                   len (p' $- p)

-- don't get stuck when executing action
noStuck :: AionBot () -> AionBot a -> AionBot a
noStuck afterUnstuck action =
    do p0 <- player_pos <$> getPlayer
       t0 <- time
       debug "trying to not get stuck now.."
       r <- withSpark (detector t0 []) $ \_ -> action
       debug "done with stuckness detection"
       return r
  where
    speed :: [ (Vec3,Float) ] -> Float
    speed []  = 0
    speed [x] = 0
    speed ((p0,t0):(p1,t1):xs) =
        speed ((p1,t1):xs) + ( if dt /= 0
                                  then dp/dt
                                  else 0
                             )
        where
          dp = len (p1 $- p0)
          dt = abs (t1 - t0)

    detector !t0 !ps =
        do delay 0.05
           p <- player_pos <$> getPlayer
           t <- time
           let !ps' = (p,t) : takeWhile ( \(p',t') -> t - t' < 2 ) ps
               !v = speed ps'
           debug $ "v=" ++ show v
           if (length ps' < 10 || t - t0 < 3)
              then detector t0 ps'
              else do if (v < 0.5)
                        then do unstuck 
                                afterUnstuck
                                t <- time
                                detector t []
                        else do detector t0 ps'

-- try to get unstuck
unstuck :: AionBot ()
unstuck =
    do info "TRYING TO GET UNSTUCK!"
       (act :: Int) <- liftIO $ randomRIO (0,3)
       timeout 3 $
               case act of
                 0 -> strafe StrafeLeft
                 1 -> strafe StrafeRight
                 2 -> backpedal
                 _ -> return ()
       info "TRIED"
       return ()


----
---- Bot state
----
data BotState = BotState { agent_channel    :: CommandChannel IO
                         , camera           :: Camera
                         , player           :: Player
                         , entities         :: [Entity]
                         , entity_map           :: Map Int Entity
                         , combat_map           :: [CombatMob]
                         , combat_last_time_ply :: Float
                         , config               :: AionBotConfig
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
       ply <- getPlayer
       ents <- catMaybes <$> mapM getEntity (map combat_id combat')
       when (length combat' > 0) $
            info $ printf "in combat with %d monsters. HP = %d : %s." (length ents) (player_hp ply) (show $ map entity_name ents)
       when (not . null $ ents) $
            do t <- time
               liftState . modify $ \s -> s { combat_last_time_ply = t }
       combatExpirator period
    where
      expire :: [CombatMob] -> CombatMob -> AionBot [CombatMob]
      expire acc m@(CombatMob id target_t0) =
          do e <- getEntity id
             let dead = case e of
                          Nothing -> True
                          Just e  -> isDeadPure e
             t <- time
             if dead || t - target_t0 > 4
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
          | entity_type e /= ENPC               = return ()
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

-- check how long since last combat
timeSinceCombat :: AionBot Float
timeSinceCombat =
    do t <- time
       s <- liftState get
       let dt = t - (combat_last_time_ply s)
       return dt

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
execute r =
    do t <- getCurrentThread
       info $ printf "thread %d executing rotation %s" t (show r)
       execute' r
       info $ printf "thread %d done with rotation." t

execute' :: Rotation -> AionBot ()
execute' r@(Repeat elems) = mapM_ execute_elem elems >> execute r
execute' r@(RepeatN 0 elems) = return ()
execute' r@(RepeatN n elems) = mapM_ execute_elem elems >> execute (RepeatN (n-1) elems)
execute' r@(Once elems)   = mapM_ execute_elem elems

execute_elem :: RotationElem -> AionBot ()
execute_elem e = liftIO (putStr "." >> hFlush stdout) >> {- debug (show e) >> -} execute_elem' e
execute_elem' :: RotationElem -> AionBot ()
execute_elem' (Delay dt) = delay dt
execute_elem' (Rotation nest) = execute nest
execute_elem' (KeyPress key) = keyPress key
execute_elem' (HoldModKeyPress mod key) =
    finally ( keyState Up mod ) $
            keyState Down mod >> keyPress key
            
-- access list of mobs in combat with player
getCombatants :: AionBot [Entity]
getCombatants =
    do s <- liftState get
       let combat = map combat_id $ combat_map s
       entities <- mapM getEntity combat
       return $ filter ( not . isDeadPure ) $ catMaybes entities

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

getPlayerHealthPercent :: AionBot Int
getPlayerHealthPercent =
    do p <- getPlayer
       return ( player_hp p * 100 `div` player_hpmax p )

-- access entity by ID
getEntity :: EntityID -> AionBot (Maybe Entity)
getEntity id = liftState get >>= \s -> return $ M.lookup id (entity_map s)

-- access current entity list
getEntities :: AionBot [Entity]
getEntities = liftState get >>= return . entities

-- access current player target
getTarget :: AionBot (Maybe Entity)
getTarget =
    do p <- getPlayerEntity
       e <- getEntity (entity_target_id p)
       case e of
         Just e | (entity_id e /= 0) -> return $ Just e
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
                  , combat_last_time_ply = -1000
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
ioHandler t0 (Trace msg) = liftIO . debugIO $ "thread> " ++ msg
--ioHandler t0 (Trace msg) = return ()
ioHandler t0 (Warn msg) = liftIO . debugIO $ "WARNING!!! thread> " ++ msg

ioDiffTime :: UTCTime -> IO Float
ioDiffTime t0 =
    do t1 <- getCurrentTime
       return . realToFrac $ diffUTCTime t1 t0

info :: String -> AionBot ()
info s = debug s >> liftIO (putStrLn s)

debug :: String -> AionBot ()
debug s = 
    do t <- getCurrentThread
       liftIO (debugIO $ show t ++ ": " ++ s)
