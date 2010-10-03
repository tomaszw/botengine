module Channel
    ( Channel
    , channelFromTCPSocket
    , channelFromUDPSocket
    , channSend
    , channRecv
    ) where

import Common

import Network.Socket ( Socket, SockAddr )
import qualified Network.Socket.ByteString as NBS
import qualified Data.ByteString as B
import Network.Socket.ByteString.Lazy ( recv )
import Data.ByteString.Lazy (ByteString, pack, unpack)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import Data.Binary ( Binary(..), Get(..), Put(..), encode, decode )
import qualified Data.Map as M
import Foreign.Storable
import Foreign.C.Types

data Channel m = Channel { channSend :: ByteString -> m ()
                         , channRecv :: Int -> m ByteString }

channelFromTCPSocket :: (MonadIO m) => Socket -> Channel m
channelFromTCPSocket s =
    Channel { channSend = \buf -> liftIO $ send_all buf
            , channRecv = \sz  -> liftIO (recv s (fromIntegral sz)) }
    where
      send_all buf =
          do num <- send buf
             let l = fromIntegral (BL.length buf)
             when (num < l) $ 
                  do let xs = BL.drop (fromIntegral num) buf
                     send_all xs
      send buf = liftIO $ fromIntegral <$> (lazy_send s buf)

      recv_all sz =
          do xs <- recv_ sz
             let l = fromIntegral $ BL.length xs
             if l < sz && l > 0
               then do ys <- recv_all (sz - l)
                       return $ BL.append xs ys
               else if l == 0
                      then error "EOF"
                      else return xs
      recv_ sz = liftIO (recv s (fromIntegral sz))


channelFromUDPSocket :: (MonadIO m) => Socket -> (Maybe SockAddr) -> Channel m
channelFromUDPSocket s maybe_addr =
    Channel { channSend = udp_send
            , channRecv = udp_recv }
    where
      udp_send buf
          | Just addr <- maybe_addr = liftIO $ NBS.sendTo s (unLazy buf) addr >> return ()
          | otherwise               = error "udp_send: unknown destination"
      udp_recv sz = liftIO $ do (buf,addr') <- NBS.recvFrom s sz
                                return $ toLazy buf
      unLazy   = B.concat . BL.toChunks
      toLazy s = BL.fromChunks [s]

lazy_send s b =
    let b' = foldl' B.append B.empty (BL.toChunks b) in
    NBS.send s b'
