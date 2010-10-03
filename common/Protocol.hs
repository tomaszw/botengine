-- mostly fixed packet size generic protocol stuff
module Protocol ( PacketCounter
                , newPacketCounter
                , sendCountedPacket, readCountedPacket
                , sendFixedSzPacket, readFixedSzPacket
                ) where

import Common
import qualified Control.Exception as E
import Data.Binary
import Data.Typeable
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import Channel

data IdentifablePacket a = IdentifablePacket { packet_id :: Integer
                                             , packet_data :: a }

instance (Binary a) => Binary (IdentifablePacket a) where
    put p = put (packet_id p) >> put (packet_data p)
    get   = get >>= \id -> get >>= \dat -> return $ IdentifablePacket id dat

data ProtocolError = TooBigPacket deriving (Show, Typeable)

instance E.Exception ProtocolError

sendPacket :: (MonadIO m, Binary b) => Channel m -> b -> m ()
sendPacket c p =
    do let buf = encode p
       channSend c buf

sendFixedSzPacket :: (MonadIO m, Binary b) => Channel m -> Int -> b -> m ()
sendFixedSzPacket c maxSz p =
    do let buf = encode p
       if BL.length buf > (fromIntegral maxSz)
          then E.throw TooBigPacket
          else channSend c buf
{-
    where
      pad buf = let diff = (fromIntegral maxSz) - BL.length buf in
                buf `BL.append` (BL.replicate diff 0)-}

readFixedSzPacket :: (MonadIO m, Binary b) => Channel m -> Int -> m b
readFixedSzPacket c maxSz =
    do buf <- channRecv c maxSz
       return $ decode buf

data PacketCounter b = PacketCounter { pc_classify :: b -> Int
                                     , pc_counters :: MVar (Map Int Integer) }

newPacketCounter :: (MonadIO m, Binary b) => (b -> Int) -> m (PacketCounter b)
newPacketCounter classify =
    do m <- liftIO $ newMVar (M.empty)
       return $ PacketCounter classify m

getMaxID :: (MonadIO m) => PacketCounter b -> Int -> m Integer
getMaxID m cls =
    do mp <- liftIO $ readMVar (pc_counters m)
       case M.lookup cls mp of
         Nothing -> return 0
         Just c  -> return c

setMaxID :: (MonadIO m) => PacketCounter b -> Int -> Integer -> m ()
setMaxID m cls id =
    liftIO $ modifyMVar_ (pc_counters m) $ \mp ->
        return (M.insert cls id mp)

-- reads newer packet from channel (id will be higher), discards older packets
readCountedPacket :: (Binary b, MonadIO m) => Channel m -> Int -> PacketCounter b -> m b
readCountedPacket c sz m =
    do packet <- readFixedSzPacket c sz
       let id  = packet_id packet
           cls = pc_classify m (packet_data packet)
       -- discard packets older than already in queue
       max <- getMaxID m cls
       if id < max
          then readCountedPacket c sz m
          else do setMaxID m cls id
                  return $ packet_data packet

-- assigns ID and sents packet through channel
sendCountedPacket :: (Binary b, MonadIO m) => Channel m -> Int -> b -> PacketCounter b -> m ()
sendCountedPacket ch sz p m =
    do let cls = pc_classify m p
       id <- liftIO $ modifyMVar (pc_counters m) $ \counters ->
           do max <- getMaxID m cls
              let id = max+1
              return (M.insert cls id counters, id)
       sendFixedSzPacket ch sz (IdentifablePacket id p)
