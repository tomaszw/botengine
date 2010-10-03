module Main where

import Common
import System.Environment
import Network.Socket

import GameState
import CmdLine
import RemoteCommand

port = 5555

run :: String -> IO ()
run addr =
    do game_state <- newGameState
       forkIO $ runUpdatesServer port game_state

       putStr "connecting to agent.... "
       hFlush stdout
       control_sock <- socket AF_INET Stream defaultProtocol
       addr' <- inet_addr addr
       connect control_sock $ SockAddrInet port addr'
       control_ch <- createGameControlChannel control_sock
       putStrLn "DONE"

       runCmdLine game_state control_ch
       
main =
    do hSetBuffering stdout LineBuffering
       args <- getArgs
       case args of
         [ addr ] -> run addr
         _        -> hPutStrLn stderr "bad args!"
