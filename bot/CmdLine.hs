module CmdLine ( runCmdLine ) where

import Common
import Text.Printf
import Control.Monad.State

import MicroThread
import Aion
import AionBot
import AionBotConfig
import GameState
import RemoteCommand
import Waypoints

data Cmd = Abort | Quit | NextTarget | AimTarget | WalkTarget | TargetInfo | PlayerInfo | EntitiesInfo | ParkMouse
         | Rotate Float | Forward Float | Jump
         | Strafe StrafeDirection
         | Walk | Backpedal
         | Kill
         | Grind
         | Pick
         | Loot
         | HealSelf
         | DefWaypoint String
         | ListWaypoints
         | Travel [String]

execCmd :: Cmd -> AionBot ()
execCmd cmd = 
    do --t0 <- time
       execCmd' cmd
       --t1 <- time
       --liftIO $ putStrLn $ printf "... finished %.2f secs" (t1-t0)

execCmd' :: Cmd -> AionBot ()
execCmd' AimTarget = aimTarget
execCmd' PlayerInfo = getPlayer >>= \p -> getPlayerEntity >>= \e -> liftIO $ putStrLn (show p) >> putStrLn (show e)
execCmd' TargetInfo = getTarget >>= \t -> liftIO $ putStrLn (show t)
execCmd' EntitiesInfo = getEntities >>= \e -> liftIO $ mapM_ (putStrLn . show) e >> putStrLn ((show $ length e) ++ " entities.")
execCmd' WalkTarget = walkToTarget
execCmd' NextTarget = nextTarget
execCmd' ParkMouse = parkMouse
execCmd' (Strafe d) = strafe d
execCmd' Grind = grind
execCmd' Jump = jump
execCmd' Walk = walk
execCmd' Backpedal = backpedal
execCmd' Kill = killTarget
execCmd' Pick = pickGrindTarget
execCmd' Loot = loot
execCmd' HealSelf = healSelf
execCmd' (Travel ws) = travel ws

execCmd' (Forward secs) = timeout secs walk >> return ()
execCmd' (Rotate a) = rotateCamera a
execCmd' _ = error "bad command"

parseCmd :: String -> Maybe Cmd
parseCmd cmd =
    let cmds = splitOn " " cmd in
    case cmds of
      [""] -> Just Abort
      ["q"] -> Just Quit
      ["aim"] -> Just AimTarget
      ["p"] -> Just PlayerInfo
      ["t"] -> Just TargetInfo
      ["e"] -> Just EntitiesInfo
      ["wt"] -> Just WalkTarget
      ["n"] -> Just NextTarget
      ["park"] -> Just ParkMouse
      ["j"] -> Just Jump
      -- plain movement keys
      ["a"] -> Just (Strafe StrafeLeft)
      ["d"] -> Just (Strafe StrafeRight)
      ["w"] -> Just Walk
      ["s"] -> Just Backpedal
      ["heal"] -> Just HealSelf
      ["pick"] -> Just Pick
      ["loot"] -> Just Loot
      ["grind"] -> Just Grind
      ["wp", name] -> Just (DefWaypoint name)
      ["wplist"] -> Just ListWaypoints
      ("travel" : ws) -> Just ( Travel ws )
      ["forward", secs_str] ->
          case reads secs_str of
            [(secs,_)] -> Just $ Forward secs
            _ -> Nothing
      ["r", v] ->
          case reads v of
            [(f,_)] -> Just $ Rotate f
            _ -> Nothing

      ["kill"] -> Just Kill
          
      _ -> Nothing


data Task = Task { abortTask :: IO ()
                 , finished  :: MVar ()
                 , threadID  :: ThreadId }

data CmdState = CmdState { game_state :: GameState
                         , channel :: CommandChannel IO
                         , background :: IORef (Maybe Task)
                         , config :: AionBotConfig }

newtype CmdLine a = CmdLine { unCmdLine :: StateT CmdState IO a }
    deriving ( Monad, Functor, MonadState CmdState, MonadIO )

printLine :: String -> CmdLine ()
printLine s = liftIO (putStrLn s)

runCmdLine :: GameState -> CommandChannel IO -> IO ()
runCmdLine gs ch =
    do bg  <- newIORef Nothing
       cfg <- readConfig
       let state = CmdState { game_state = gs
                            , channel = ch
                            , background = bg
                            , config = cfg
                            }
       evalStateT (unCmdLine runCmdLine') state

runCmdLine' :: CmdLine ()
runCmdLine' =
    do quit <- cmdLine
       case quit of
         True  -> printLine "quitting command line." >> return ()
         False -> runCmdLine'

cmdLine :: CmdLine Bool
cmdLine =
    do liftIO $ putStr ">> " >> hFlush stdout
       line <- liftIO getLine
       state <- get
       let gs  = game_state state
           c   = channel state
           cmd = parseCmd line
       case cmd of
         Nothing   -> printLine "invalid command." >> return False
         Just Quit -> return True
         Just Abort -> backgroundJobAbort >> return False
         Just (DefWaypoint name) -> defineWaypoint name >> return False
         Just ListWaypoints -> listWaypoints >> return False
         Just PlayerInfo -> liftIO (runAionBot c gs (config state) (execCmd PlayerInfo)) >> return False
         Just TargetInfo -> liftIO (runAionBot c gs (config state) (execCmd TargetInfo)) >> return False
         Just EntitiesInfo -> liftIO (runAionBot c gs (config state) (execCmd EntitiesInfo)) >> return False
         Just cmd -> backgroundJob (execCmd cmd) >> return False

backgroundJob :: AionBot () -> CmdLine ()
backgroundJob action =
    do backgroundJobAbort -- abort previous if any
       state <- get
       (id, abort_fun, finished_var) <- liftIO $ runAbortableAionBot (channel state) (game_state state) (config state) action
       liftIO $ writeIORef (background state) $ 
                  Just $ Task { abortTask = abort_fun
                              , finished  = finished_var
                              , threadID  = id }

backgroundJobAbort :: CmdLine ()
backgroundJobAbort =
    do state <- get
       liftIO $ do
         bg <- readIORef (background state)
         case bg of
           Nothing   -> return ()
           Just task ->
               do abortTask task
                  takeMVar (finished task)
                  writeIORef (background state) Nothing

defineWaypoint :: String -> CmdLine ()
defineWaypoint name =
    do s <- get
       player <- liftIO $ readIORef (game_player $ game_state s)
       let wp   = Waypoint name (player_pos player)
           cfg  = config s
           cfg' = cfg { waypoints = waypoints cfg ++ [wp] }
       modify $ \s -> s { config = cfg' }
       liftIO $ saveConfig cfg'

listWaypoints :: CmdLine ()
listWaypoints =
    do s <- get
       let cfg = config s
       mapM_ print (waypoints cfg)
    where
      print p = printLine (show p)
