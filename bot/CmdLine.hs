module CmdLine ( runCmdLine ) where

import Data.List
import Data.List.Split
import Control.Concurrent
import Control.Monad
import Control.Monad.Trans
import Control.Applicative
import Text.Printf

import System.IO

import AionBot
import MicroThread
import Comm
import GameState

data Cmd = Quit | NextTarget | AimTarget | WalkTarget | TargetInfo | PlayerInfo | EntitiesInfo | ParkMouse
         | Rotate Float

execCmd :: GameState -> Cmd -> AionBot ()
execCmd gs cmd = 
    do t0 <- time
       updateState gs
       withSpark (delay 0.01 >> stateReader gs 0.01) $ \_ -> execCmd' cmd
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
execCmd' (Rotate a) = rotateCamera a

execCmdWithGameWindow c gs cmd =
    do w <- getForegroundWindow c
       g <- getGameWindow c
       setForegroundWindow c g
       threadDelay (10^5 * 1)
       runAionBot c $ execCmd gs cmd
       threadDelay (10^5 * 1)
       setForegroundWindow c w

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
      ["rot", v] ->
          case reads v of
            [(f,_)] -> Just $ Rotate f
            _ -> Nothing

      _ -> Nothing

runCmdLine :: GameState -> Channel IO -> IO ()
runCmdLine gs c =
    do putStr ">> "
       hFlush stdout
       cmd <- parseCmd <$> getLine
       case cmd of
         Nothing   -> putStrLn "invalid command." >> hFlush stdout >> runCmdLine gs c
         Just Quit -> return ()
         Just PlayerInfo -> runAionBot c (execCmd gs PlayerInfo) >> runCmdLine gs c
         Just TargetInfo -> runAionBot c (execCmd gs TargetInfo) >> runCmdLine gs c
         Just EntitiesInfo -> runAionBot c (execCmd gs EntitiesInfo) >> runCmdLine gs c
         Just cmd -> execCmdWithGameWindow c gs cmd >> runCmdLine gs c
