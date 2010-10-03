module CmdLine ( runCmdLine ) where

import Common
import Text.Printf

import MicroThread
import AionBot
import GameState
import RemoteCommand

data Cmd = Quit | NextTarget | AimTarget | WalkTarget | TargetInfo | PlayerInfo | EntitiesInfo | ParkMouse
         | Rotate Float | Forward Float

execCmd :: Cmd -> AionBot ()
execCmd cmd = 
    do t0 <- time
       execCmd' cmd
       t1 <- time
       liftIO $ putStrLn $ printf "... finished %.2f secs" (t1-t0)

execCmd' :: Cmd -> AionBot ()
execCmd' AimTarget = aimTarget
execCmd' PlayerInfo = getPlayer >>= \p -> getPlayerEntity >>= \e -> liftIO $ putStrLn (show p) >> putStrLn (show e)
execCmd' TargetInfo = getTarget >>= \t -> liftIO $ putStrLn (show t)
execCmd' EntitiesInfo = getEntities >>= \e -> liftIO $ mapM_ (putStrLn . show) e >> putStrLn ((show $ length e) ++ " entities.")
execCmd' WalkTarget = walkToTarget 10
execCmd' NextTarget = nextTarget
execCmd' ParkMouse = parkMouse
execCmd' (Forward secs) = timeout secs $ walk
execCmd' (Rotate a) = rotateCamera a
execCmd' _ = error "bad command"

parseCmd :: String -> Maybe Cmd
parseCmd cmd =
    let cmds = splitOn " " cmd in
    case cmds of
      ["q"] -> Just Quit
      ["aim"] -> Just AimTarget
      ["p"] -> Just PlayerInfo
      ["t"] -> Just TargetInfo
      ["e"] -> Just EntitiesInfo
      ["wt"] -> Just WalkTarget
      ["n"] -> Just NextTarget
      ["park"] -> Just ParkMouse
      ["forward", secs_str] ->
          case reads secs_str of
            [(secs,_)] -> Just $ Forward secs
            _ -> Nothing
      ["rot", v] ->
          case reads v of
            [(f,_)] -> Just $ Rotate f
            _ -> Nothing

      _ -> Nothing

runCmdLine :: GameState -> CommandChannel IO -> IO ()
runCmdLine gs c =
    do putStr ">> "
       hFlush stdout
       cmd <- parseCmd <$> getLine
       case cmd of
         Nothing   -> putStrLn "invalid command." >> hFlush stdout >> runCmdLine gs c
         Just Quit -> return ()
         Just PlayerInfo -> runAionBot c gs (execCmd PlayerInfo) >> runCmdLine gs c
         Just TargetInfo -> runAionBot c gs (execCmd TargetInfo) >> runCmdLine gs c
         Just EntitiesInfo -> runAionBot c gs (execCmd EntitiesInfo) >> runCmdLine gs c
         Just cmd -> runAionBot c gs (execCmd cmd) >> runCmdLine gs c
