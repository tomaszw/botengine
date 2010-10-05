module CmdLine ( runCmdLine ) where

import Common
import Text.Printf

import MicroThread
import AionBot
import GameState
import RemoteCommand

data Cmd = Abort | Quit | NextTarget | AimTarget | WalkTarget | TargetInfo | PlayerInfo | EntitiesInfo | ParkMouse
         | Rotate Float | Forward Float | Jump
         | Strafe StrafeDirection
         | Walk | Backpedal
         | Kill
         | Grind
         | Pick
         | Loot
         | HealSelf
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
execCmd' WalkTarget = walkToTarget 10
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
                         , background :: IORef (Maybe Task) }

runCmdLine :: GameState -> CommandChannel IO -> IO ()
runCmdLine gs ch =
    do bg <- newIORef Nothing
       let state = CmdState { game_state = gs
                            , channel = ch
                            , background = bg }
       runCmdLine' state

runCmdLine' :: CmdState -> IO ()
runCmdLine' cmd_state =
    do quit <- cmdLine cmd_state
       case quit of
         True  -> putStrLn "quitting command line." >> return ()
         False -> runCmdLine' cmd_state

cmdLine :: CmdState -> IO Bool
cmdLine state =
    do putStr ">> "
       hFlush stdout
       line <- getLine
       let gs  = game_state state
           c   = channel state
           cmd = parseCmd line
       case cmd of
         Nothing   -> putStrLn "invalid command." >> hFlush stdout >> return False
         Just Quit -> return True
         Just Abort -> backgroundJobAbort state >> return False
         Just PlayerInfo -> runAionBot c gs (execCmd PlayerInfo) >> return False
         Just TargetInfo -> runAionBot c gs (execCmd TargetInfo) >> return False
         Just EntitiesInfo -> runAionBot c gs (execCmd EntitiesInfo) >> return False
         Just cmd -> backgroundJob state (execCmd cmd) >> return False

backgroundJob :: CmdState -> AionBot () -> IO ()
backgroundJob state action =
    do backgroundJobAbort state -- abort previous if any
       (id, abort_fun, finished_var) <- runAbortableAionBot (channel state) (game_state state) action
       writeIORef (background state) $ 
                  Just $ Task { abortTask = abort_fun
                              , finished  = finished_var
                              , threadID  = id }

backgroundJobAbort :: CmdState -> IO ()
backgroundJobAbort state =
    do bg <- readIORef (background state)
       case bg of
         Nothing   -> return ()
         Just task ->
             do abortTask task
                takeMVar (finished task)
                writeIORef (background state) Nothing




