module Main where

import Control.Monad
import Control.Monad.Trans
import qualified Control.Exception as E
import Network.Socket
import System.IO
import Comm
import Aion
import Process
import WinProcess

port = 5555

conversation :: WinProcess -> Channel IO -> IO ()
conversation p c = 
    do code <- channRecvBinary c
       let cmd = commFromCode code
       handle cmd
       conversation p c
    where
      handle _ = error "unknown command"
    
server :: WinProcess -> IO ()
server p =
    do sock <- socket AF_INET Stream defaultProtocol
       bindSocket sock (SockAddrInet port iNADDR_ANY)
       listen sock 1
       putStrLn "listening.."
       loop sock
    where
      loop sock = do (client,addr) <- accept sock
                     putStrLn $ "client connected from " ++ show addr
                     conversation p (channelFromSocket client) `E.catch` err client
                     disconnect
          where
            err :: Socket -> E.SomeException -> IO ()
            err c e = putStrLn (show e) >> (E.try (sClose c) :: IO (Either E.SomeException ())) >> disconnect
            disconnect = putStrLn "client disconnected." >> loop sock

main :: IO ()
main = do p <- openGameProcess "AION Client"
          withSocketsDo (server p)
          return ()
