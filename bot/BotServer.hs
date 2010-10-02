module BotServer where

import Network.Socket
import Comm

runBotServer :: Int -> IO ()
runBotServer port =
    do sock <- socket AF_INET Stream defaultProtocol
       bindSocket sock $ SockAddrInet (portNum $ fromIntegral port) (inet_addr "127.0.0.1")
       accept_loop sock

  where
    accept_loop sock =
        do (clientSock, clientAddr) <- accept sock
           forkIO $ conversation clientSock
           accept_loop sock

    conversation sock =
        do ch <- channelFromSocket sock
           return ()