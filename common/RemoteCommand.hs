module RemoteCommand where

import Common
import Network.Socket ( Socket, SockAddr )
import qualified Network.Socket.ByteString as NBS
import qualified Data.ByteString as B
import Network.Socket.ByteString.Lazy ( recv )
import Data.ByteString.Lazy (ByteString, pack, unpack)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import Data.Binary ( Binary(..), Get(..), Put(..), encode, decode )
import Data.Binary.IEEE754
import qualified Data.Map as M
import Foreign.Storable
import Foreign.C.Types

import Channel ( KeyState, KeyCode, Button )

data Command = ChangeKeyState KeyState KeyCode
             | ChangeMouseState KeyState Button
             | RelMoveMouse Int32 Int32
             | AbsMoveMousePerc Float Float
             | OrientPlayer Float
             | QueryCamera
             | QueryPlayer
             | QueryEntityList
             | GetForegroundWindow
             | SetForegroundWindow Word32
             | GetGameWindow

instance Binary Command where
    put (ChangeKeyState s c) = put (1 :: Word8) >> put s >> put c
    put (ChangeMouseState x y) = put (2 :: Word8) >> put x >> put y
    put (RelMoveMouse x y) = put (3 :: Word8) >> put x >> put y
    put (AbsMoveMousePerc x y) = put (4 :: Word8) >> putFloat32be x >> putFloat32be y
    put (OrientPlayer r) = put (5 :: Word8) >> putFloat32be r
    put (QueryCamera) = put (6 :: Word8)
    put (QueryPlayer) = put (7 :: Word8)
    put (QueryEntityList) = put (8 :: Word8)
    put (GetForegroundWindow) = put (9 :: Word8)
    put (SetForegroundWindow w) = put (10 :: Word8) >> put w
    put (GetGameWindow) = put (11 :: Word8)

    get = do (c :: Word8) <- get
             case c of
               1 -> do { s <- get; c <- get; return $ ChangeKeyState s c }
               2 -> do { s <- get; b <- get; return $ ChangeMouseState s b }
               3 -> do { x <- get; y <- get; return $ RelMoveMouse x y }
               4 -> do { x <- getFloat32be; y <- getFloat32be; return $ AbsMoveMousePerc x y }
               5 -> do { r <- getFloat32be; return $ OrientPlayer r }
               6 -> return QueryCamera
               7 -> return QueryPlayer
               8 -> return QueryEntityList
               9 -> return GetForegroundWindow
               10 -> do { w <- get; return $ SetForegroundWindow w }
               11 -> return GetGameWindow
