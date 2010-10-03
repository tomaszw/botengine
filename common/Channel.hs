module Channel ( Channel
            , KeyState(..), Button(..), KeyCode
            , Command (..), commCode, commFromCode
            , channelFromSocket
            , channelFromUDPSocket
            , getModuleHandle
            , getGameWindow
            , getForegroundWindow
            , setForegroundWindow
            , readProcessMemory
            , setMousePos
            , sendKey, sendKeyPress, sendMouseBtn, sendMouseClick, sendMouseMove
            , channSendA
            , channRecvA
            , channSendBinary
            , channRecvBinary'
            , recvPlayer
            , recvEntityList
            , recvCamera
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

import Aion

data Channel m = Channel { channSend :: ByteString -> m Int
                         , channRecv :: Int -> m ByteString }

channelFromSocket :: (MonadIO m) => Socket -> Channel m
channelFromSocket s = Channel { channSend = \buf -> liftIO $ fromIntegral <$> (lazy_send s buf)
                              , channRecv = \sz  -> liftIO (recv s (fromIntegral sz)) }

channelFromUDPSocket :: (MonadIO m) => Socket -> SockAddr -> Channel m
channelFromUDPSocket s addr =
    Channel { channSend = udp_send
            , channRecv = udp_recv }
    where
      udp_send buf = liftIO $ NBS.sendTo s (unLazy buf) addr
      udp_recv sz  = liftIO $ do (buf,addr') <- NBS.recvFrom s sz
                                 return $ toLazy buf
      unLazy   = B.concat . BL.toChunks
      toLazy s = BL.fromChunks [s]

data Command = GetModuleHandle
         | ReadProcessMemory
         | SendKey
         | SendMouse
         | SendMouseMove
         | SetMousePos
         | SetPlayerRot
         | GetForegroundWindow
         | SetForegroundWindow
         | GetGameWindow
         | GetCamera
         | GetPlayer
         | GetEntityList
           deriving (Eq, Ord, Show)

commCodes = [ (GetModuleHandle, 1)
            , (ReadProcessMemory, 2)
            , (SendKey, 3)
            , (SetMousePos, 4)
            , (SetPlayerRot, 5)
            , (SetMousePos, 6)
            , (GetForegroundWindow, 7)
            , (SetForegroundWindow, 8)
            , (GetGameWindow, 9)
            , (GetCamera, 10)
            , (GetPlayer, 11)
            , (GetEntityList, 12)
            , (SendMouseMove, 13)
            ]

commCode :: Command -> Word8
commCode =
    let m = M.fromList commCodes in
    \cmd ->
        let Just v = M.lookup cmd m in
        v

commFromCode :: Word8 -> Command
commFromCode =
    let m = M.fromList codes'
        codes' = map (\(a,b) -> (b,a)) commCodes in
    \code ->
        let Just v = M.lookup code m in
        v

lazy_send s b =
    let b' = foldl' B.append B.empty (BL.toChunks b) in
    NBS.send s b'

channSendA :: (Monad m) => Channel m -> ByteString -> m ()
channSendA c buf =
    do num <- channSend c buf
       let l = fromIntegral (BL.length buf)
       when (num < l) $ 
            do let xs = BL.drop (fromIntegral num) buf
               channSendA c xs

channRecvA :: (Monad m) => Channel m -> Int -> m ByteString
channRecvA c sz =
    do xs <- channRecv c sz
       let l = fromIntegral $ BL.length xs
       if l < sz && l > 0
          then do ys <- channRecvA c (sz - l)
                  return $ BL.append xs ys
          else if l == 0
                  then error "EOF"
                  else return xs

channSendBinary :: (Monad m, Binary b) => Channel m -> b -> m ()
channSendBinary c v = channSendA c (encode v)

channRecvBinary_ :: (Monad m, Binary b, Storable b) => Channel m -> b -> m b
channRecvBinary_ c v = let l = sizeOf v in
                    do buf <- channRecvA c l
                       return $ decode buf
channRecvBinary c = channRecvBinary_ c undefined

channRecvBinary' :: (Monad m, Binary b) => Channel m -> Int -> m b
channRecvBinary' c len =
    do buf <- channRecvA c len
       return $ decode buf

recvCamera :: (Monad m) => Channel m -> m Camera
recvCamera c =
    do channSendBinary c $ commCode GetCamera
       len <- channRecvBinary c
       channRecvBinary' c (fromIntegral (len::Word32))

recvPlayer :: (Monad m) => Channel m -> m Player
recvPlayer c =
    do channSendBinary c $ commCode GetPlayer
       len <- channRecvBinary c
       channRecvBinary' c (fromIntegral (len::Word32))

recvEntityList :: (Monad m) => Channel m -> m [Entity]
recvEntityList c =
    do channSendBinary c $ commCode GetEntityList
       count <- channRecvBinary c
       aux (fromIntegral (count::Word32))
    where
      aux 0 = return []
      aux n =
          do len <- channRecvBinary c
             e <- channRecvBinary' c (fromIntegral (len::Word32))
             es <- aux (n-1)
             return $ e:es

data KeyState = Up | Down
data Button = L | R
type KeyCode = Word32

instance Binary KeyState where
    put Up = put (0 :: Word8)
    put Down = put (1 :: Word8)
    get = do c <- get :: Get Word8
             case c of { 0 -> return Up; 1 -> return Down }

instance Binary Button where
    put L = put (0 :: Word8)
    put R = put (1 :: Word8)
    get = do c <- get :: Get Word8
             case c of { 0 -> return L; 1 -> return R }

sendString :: (Monad m) => Channel m -> String -> m ()
sendString c s =
    do channSendA c (UTF8.fromString s)
       channSendBinary c $ ( 0 :: Word8 )

getModuleHandle :: (Monad m) => Channel m -> String -> m Word32
getModuleHandle c name =
    do channSendBinary c $ commCode GetModuleHandle
       sendString c name
       h <- channRecvBinary c
       return h

getGameWindow :: (Monad m) => Channel m -> m Word32
getGameWindow c =
    do channSendBinary c $ commCode GetGameWindow
       channRecvBinary c

getForegroundWindow :: (Monad m) => Channel m -> m Word32
getForegroundWindow c =
    do channSendBinary c $ commCode GetForegroundWindow
       channRecvBinary c

setForegroundWindow :: (Monad m) => Channel m -> Word32 -> m ()
setForegroundWindow c hwnd =
    do channSendBinary c $ commCode SetForegroundWindow
       channSendBinary c hwnd

readProcessMemory :: (Monad m) => Channel m -> Word32 -> Int -> m ByteString
readProcessMemory c offset len =
    do channSendBinary c $ commCode ReadProcessMemory
       channSendBinary c offset
       channSendBinary c (fromIntegral len :: Word32)
       channRecvA c len

setMousePos :: (Monad m) => Channel m -> Float -> Float -> m ()
setMousePos c x y =
    let x' = (round $ x * 100) :: Word32
        y' = (round $ y * 100) :: Word32 in
    do channSendBinary c $ commCode SetMousePos
       channSendBinary c x'
       channSendBinary c y'

sendKey :: (Monad m) => Channel m -> KeyState -> KeyCode -> m ()
sendKey c s code =
    do channSendBinary c $ commCode SendKey
       channSendBinary c s
       channSendBinary c code

sendKeyPress :: (Monad m) => Channel m -> KeyCode -> m ()
sendKeyPress c code =
    do sendKey c Down code
       sendKey c Up code

sendMouseBtn :: (Monad m) => Channel m -> KeyState -> Button -> m ()
sendMouseBtn c s b =
    do channSendBinary c $ commCode SendMouse
       channSendBinary c s
       channSendBinary c b

sendMouseClick :: (Monad m) => Channel m -> Button -> m ()
sendMouseClick c b =
    do sendMouseBtn c Down b
       sendMouseBtn c Up b

sendMouseMove :: (Monad m) => Channel m -> Word32 -> Word32 -> m ()
sendMouseMove c dx dy =
    do channSendBinary c $ commCode SendMouseMove
       channSendBinary c dx
       channSendBinary c dy
