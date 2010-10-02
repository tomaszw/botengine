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

data Cmd = Quit | AimTarget | TargetInfo | PlayerInfo | EntitiesInfo


execCmd cmd = 
    do t0 <- time
       updateState
       execCmd' cmd
       t1 <- time
       liftIO $ putStrLn $ printf "... finished %.2f secs" (t1-t0)

execCmd' :: Cmd -> AionBot ()
execCmd' AimTarget = aimTarget
execCmd' PlayerInfo = getPlayer >>= \p -> getPlayerEntity >>= \e -> liftIO $ putStrLn (show p) >> putStrLn (show e)
execCmd' TargetInfo = getTarget >>= \t -> liftIO $ putStrLn (show t)
execCmd' EntitiesInfo = getEntities >>= \e -> liftIO $ mapM_ (putStrLn . show) e >> putStrLn ((show $ length e) ++ " entities.")

execCmdWithGameWindow c cmd =
    do w <- getForegroundWindow c
       g <- getGameWindow c
       setForegroundWindow c g
       threadDelay (10^5 * 1)
       runAionBot c $ execCmd cmd
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
      _ -> Nothing

runCmdLine :: Channel IO -> IO ()
runCmdLine c =
    do putStr ">> "
       hFlush stdout
       cmd <- parseCmd <$> getLine
       case cmd of
         Nothing   -> putStrLn "invalid command." >> hFlush stdout >> runCmdLine c
         Just Quit -> return ()
         Just PlayerInfo -> runAionBot c (execCmd PlayerInfo) >> runCmdLine c
         Just TargetInfo -> runAionBot c (execCmd TargetInfo) >> runCmdLine c
         Just EntitiesInfo -> runAionBot c (execCmd EntitiesInfo) >> runCmdLine c
         Just cmd -> execCmdWithGameWindow c cmd >> runCmdLine c
