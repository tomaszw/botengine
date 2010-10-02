module Main where

import BotClient
import CmdLine
import System.IO
import System.Environment

main =
    do hSetBuffering stdout LineBuffering
       args <- getArgs
       case args of
         [ addr ] -> do
                   putStrLn "connecting.."
                   c <- connectBot addr
                   putStrLn "connected."
                   runCmdLine c
         _ ->
             hPutStrLn stderr "bad args"
