module Packet where

import Control.Concurrent
import Data.Word
import Data.Binary
import Network.Socket ( socket, Socket, SockAddr )
import Network.Socket.ByteString
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import System.IO.Unsafe

import Aion

data Counter = Counter { maxID :: MVar Integer }

newCounter :: IO Counter
newCounter =
    do m <- newMVar 0
       return $ Counter m

withCounter :: Packet -> Counter -> IO () -> IO ()
withCounter p c act =
    do mx <- takeMVar (maxID c)
       if id > mx
          then putMVar (maxID c) id >> act
          else putMVar (maxID c) mx
    where
      id = packet_id p

newPacketUsingCounter :: Counter -> PacketData -> IO Packet
newPacketUsingCounter c d =
    do mx <- takeMVar (maxID c)
       let p = Packet { packet_id = mx+1
                      , packet_addr = Nothing
                      , packet_data = d }
       putMVar (maxID c) (mx+1)
       return p

data Packet = Packet { packet_id :: Integer
                     , packet_addr :: Maybe SockAddr
                     , packet_data :: PacketData }

data PacketData = UpdateCamera Camera
                | UpdatePlayer Player
                | UpdateEntityList [Entity]

instance Binary Packet where
    put p = do put $ packet_id p
               put $ packet_data p
    get   = do id <- get
               dat <- get
               return $ Packet { packet_id = id, packet_addr = Nothing, packet_data = dat }

instance Binary PacketData where
    put (UpdateCamera c) = put (0 :: Word8) >> put c
    put (UpdatePlayer p) = put (1 :: Word8) >> put p
    put (UpdateEntityList l) = put (2 :: Word8) >> put l
    get =
        do t <- get :: Get Word8
           get' t
        where
          get' 0 = get >>= \c -> return $ UpdateCamera c
          get' 1 = get >>= \p -> return $ UpdatePlayer p
          get' 2 = get >>= \l -> return $ UpdateEntityList l
    
maxPacketSz :: Int
maxPacketSz = 32768

sendPacket :: Socket -> SockAddr -> Packet -> IO ()
sendPacket sock addr p =
    do let buf = B.concat . BL.toChunks $ encode p
       sendTo sock buf addr
       return ()

receivePackets :: Socket -> IO [Packet]
receivePackets sock = receive
    where
      receive :: IO [Packet]
      receive = do
        (buf, addr) <- recvFrom sock maxPacketSz
        let buf' = BL.fromChunks [buf]
            p = decode buf'
        ps <- unsafeInterleaveIO receive
        return (p:ps)