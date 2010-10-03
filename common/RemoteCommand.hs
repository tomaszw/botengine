module RemoteCommand where

import Common
import Data.Binary ( Binary(..), Get(..), Put(..), encode, decode )
import Data.Binary.IEEE754
import Network.Socket

import Channel ( KeyState, KeyCode, Button, Channel, channelFromUDPSocket, channelFromTCPSocket )
import Protocol

data CommandChannel m = CommandChannel { channel      :: Channel m
                                       , datagram_sz  :: Int
                                       , counter      :: Maybe (PacketCounter Command)
                                       }

data Command = ChangeKeyState KeyState KeyCode
             | ChangeMouseState KeyState Button
             | RelMoveMouse Int32 Int32
             | AbsMoveMousePerc Float Float
             | OrientPlayer Float
             | UpdateCamera
             | UpdatePlayer
             | UpdateEntityList
             | GetForegroundWindow
             | SetForegroundWindow Word32
             | GetGameWindow

-- helper for packet classifier
classify :: Command -> Int
classify (ChangeKeyState _ _) = 0
classify (ChangeMouseState _ _) = 1
classify (RelMoveMouse _ _) = 2
classify (AbsMoveMousePerc _ _) = 3
classify (OrientPlayer _) = 4
classify (UpdateCamera) = 5
classify (UpdatePlayer) = 6
classify (UpdateEntityList) = 7
classify (GetForegroundWindow) = 8
classify (SetForegroundWindow _) = 9
classify (GetGameWindow) = 10

instance Binary Command where
    put (ChangeKeyState s c) = put (1 :: Word8) >> put s >> put c
    put (ChangeMouseState x y) = put (2 :: Word8) >> put x >> put y
    put (RelMoveMouse x y) = put (3 :: Word8) >> put x >> put y
    put (AbsMoveMousePerc x y) = put (4 :: Word8) >> putFloat32be x >> putFloat32be y
    put (OrientPlayer r) = put (5 :: Word8) >> putFloat32be r
    put (UpdateCamera) = put (6 :: Word8)
    put (UpdatePlayer) = put (7 :: Word8)
    put (UpdateEntityList) = put (8 :: Word8)
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
               6 -> return UpdateCamera
               7 -> return UpdatePlayer
               8 -> return UpdateEntityList
               9 -> return GetForegroundWindow
               10 -> do { w <- get; return $ SetForegroundWindow w }
               11 -> return GetGameWindow

sendCommand :: (MonadIO m) => CommandChannel m -> Command -> m ()
sendCommand (CommandChannel ch sz cnt) cmd
    | Just counter <- cnt = sendCountedPacket ch sz cmd counter
    | otherwise           = sendFixedSzPacket ch sz cmd

-- we use udp for bulk game state updates
createStateUpdatesChannel :: (MonadIO m) => Socket -> Maybe SockAddr -> m (CommandChannel m)
createStateUpdatesChannel sock addr =
    do c  <- newPacketCounter classify
       let ch = channelFromUDPSocket sock addr
       return $ CommandChannel { channel     = ch
                               , datagram_sz = 16384
                               , counter = Just c }

-- we use tcp for small control events, assume socket is already connected
createGameControlChannel :: (MonadIO m) => Socket -> m (CommandChannel m)
createGameControlChannel sock  =
    do let ch = channelFromTCPSocket sock
       return $ CommandChannel { channel     = ch
                               , datagram_sz = 64
                               , counter     = Nothing }
