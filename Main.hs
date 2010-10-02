module Main where

import BotClient
import CmdLine
import System.IO

main =
    do hSetBuffering stdout LineBuffering
       putStrLn "connecting.."
       c <- connectBot
       putStrLn "connected."
       runCmdLine c
