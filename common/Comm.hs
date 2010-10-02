module Comm ( Channel
            , KeyState(..), Button(..), KeyCode
            , Command (..), commCode, commFromCode
            , channelFromSocket
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

import Network.Socket ( Socket )
import qualified Network.Socket.ByteString as NBS
import qualified Data.ByteString as StrictBs
import Network.Socket.ByteString.Lazy ( recv )
import Data.Word
import Data.Int
import Data.List
import Data.Bits
import Data.ByteString.Lazy (ByteString, pack, unpack)
import qualified Data.ByteString.Lazy as Bs
import Control.Monad
import Control.Monad.Trans
import Control.Applicative
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import Data.Binary ( Binary(..), Get(..), Put(..), encode, decode )
import Foreign.Storable
import Foreign.C.Types
import Debug.Trace
import System.IO.Unsafe

import Aion

data Channel m = Channel { channSend :: ByteString -> m Int
                         , channRecv :: Int -> m ByteString }

channelFromSocket :: (MonadIO m) => Socket -> Channel m
channelFromSocket s = Channel { channSend = \buf -> liftIO $ fromIntegral <$> (lazy_send s buf)
                              , channRecv = \sz  -> liftIO (recv s (fromIntegral sz)) }

lazy_send s b =
    let b' = foldl' StrictBs.append StrictBs.empty (Bs.toChunks b) in
    NBS.send s b'

channSendA :: (Monad m) => Channel m -> ByteString -> m ()
channSendA c buf =
    do num <- channSend c buf
       let l = fromIntegral (Bs.length buf)
       when (num < l) $ 
            do let xs = Bs.drop (fromIntegral num) buf
               channSendA c xs

channRecvA :: (Monad m) => Channel m -> Int -> m ByteString
channRecvA c sz =
    do xs <- channRecv c sz
       let l = fromIntegral $ Bs.length xs
       if l < sz && l > 0
          then do ys <- channRecvA c (sz - l)
                  return $ Bs.append xs ys
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

data Command = GetModuleHandle
         | ReadProcessMemory
         | SendKey
         | SendMouse
         | SendMouseMove
         | SetMousePos
         | GetForegroundWindow
         | SetForegroundWindow
         | GetGameWindow
         | GetCamera
         | GetPlayer
         | GetEntityList
           deriving (Eq, Show)

commCode :: Command -> Word8
commCode GetModuleHandle = 1
commCode ReadProcessMemory = 2
commCode SendKey = 3
commCode SendMouse = 4
commCode SetMousePos = 6
commCode GetForegroundWindow = 7
commCode SetForegroundWindow = 8
commCode GetGameWindow = 9
commCode GetCamera = 10
commCode GetPlayer = 11
commCode GetEntityList = 12
commCode SendMouseMove = 13

commFromCode :: Word8 -> Command
commFromCode 1 = GetModuleHandle
commFromCode 2 = ReadProcessMemory
commFromCode 3 = SendKey
commFromCode 4 = SendMouse
commFromCode 6 = SetMousePos
commFromCode 7 = GetForegroundWindow
commFromCode 8 = SetForegroundWindow
commFromCode 9 = GetGameWindow
commFromCode 10 = GetCamera
commFromCode 11 = GetPlayer
commFromCode 12 = GetEntityList
commFromCode 13 = SendMouseMove

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
    let x' = round $ x * 100 :: Word32
        y' = round $ y * 100 :: Word32 in
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

