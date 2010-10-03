module BotService ( connectAgent, port, runGameUpdater ) where

import Control.Monad
import Control.Monad.Trans
import Control.Concurrent
import Network.Socket
import System.IO

import Channel
import Aion
import Packet
import GameState

port = 5555

handlePacket :: GameState -> Packet -> IO ()
handlePacket gs p =
    updateGameState p gs

serviceSocket :: GameState -> Socket -> IO ()
serviceSocket gs sock =
    do packets <- receivePackets sock
       mapM_ (handlePacket gs) packets
       serviceSocket gs sock

runGameUpdater :: GameState -> IO ()
runGameUpdater gs =
    do recvSock <- socket AF_INET Datagram defaultProtocol
       bindSocket recvSock (SockAddrInet port iNADDR_ANY)
       forkIO $ serviceSocket gs recvSock
       return ()

connectAgent :: (MonadIO m) => String -> m (Channel m)
connectAgent addr = liftIO $ do
       sock <- socket AF_INET Stream defaultProtocol
       a <- inet_addr addr
       connect sock $ SockAddrInet port a
       return $ channelFromSocket sock
