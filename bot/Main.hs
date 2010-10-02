module Main where

import Control.Concurrent
import System.IO
import System.Environment

import GameState
import BotService
import CmdLine


main =
    do hSetBuffering stdout LineBuffering
       args <- getArgs
       case args of
         [ addr ] -> do
                   putStrLn "connecting to agent.."
                   c <- connectAgent addr
                   putStrLn "connected."
                   gs <- newGameState
                   -- fork the state updater
                   forkIO $ runGameUpdater gs
                   runCmdLine gs c
         _ ->
             hPutStrLn stderr "bad args"
