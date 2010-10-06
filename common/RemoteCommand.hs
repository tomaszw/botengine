module RemoteCommand
    ( CommandChannel
    , Command (..)
    , KeyState (..), Button (..), KeyCode
    , sendCommand, readCommand
    , createStateUpdatesChannel
    , createGameControlChannel
    ) where

import Common
import Data.Binary ( Binary(..), Get(..), Put(..), encode, decode )
import Data.Binary.IEEE754
import Network.Socket
import Aion

import Channel
import Protocol

data KeyState = Up | Down deriving ( Eq, Show )
data Button = L | R deriving ( Eq, Show )
type KeyCode = Word32

instance Binary KeyState where
    put Up = put (0 :: Word8)
    put Down = put (1 :: Word8)
    get = do c <- get :: Get Word8
             case c of { 0 -> return Up; 1 -> return Down; _ -> error "error reading key state" }

instance Binary Button where
    put L = put (0 :: Word8)
    put R = put (1 :: Word8)
    get = do c <- get :: Get Word8
             case c of { 0 -> return L; 1 -> return R; _ -> error "error reading button" }

data CommandChannel m = CommandChannel {
                                         ch_send      :: Command -> m ()
                                       , ch_read      :: m Command
                                       }

data Command = ChangeKeyState KeyState KeyCode
             | ChangeMouseState KeyState Button
             | RelMoveMouse Int32 Int32
             | AbsMoveMousePerc Float Float
             | OrientCamera Float
             | UpdateCamera Camera
             | UpdatePlayer Player
             | UpdateEntities [Entity]
             | GetForegroundWindow
             | SetForegroundWindow Word32
             | GetGameWindow
               deriving ( Show )
-- helper for packet classifier
classify :: Command -> Int
classify (ChangeKeyState _ _) = 0
classify (ChangeMouseState _ _) = 1
classify (RelMoveMouse _ _) = 2
classify (AbsMoveMousePerc _ _) = 3
classify (OrientCamera _) = 4
classify (UpdateCamera _) = 5
classify (UpdatePlayer _) = 6
classify (UpdateEntities _) = 7
classify (GetForegroundWindow) = 8
classify (SetForegroundWindow _) = 9
classify (GetGameWindow) = 10

instance Binary Command where
    put (ChangeKeyState s c) = put (1 :: Word8) >> put s >> put c
    put (ChangeMouseState s c) = put (2 :: Word8) >> put s >> put c
    put (RelMoveMouse x y) = put (3 :: Word8) >> put x >> put y
    put (AbsMoveMousePerc x y) = put (4 :: Word8) >> putFloat32be x >> putFloat32be y
    put (OrientCamera r) = put (5 :: Word8) >> putFloat32be r
    put (UpdateCamera c) = put (6 :: Word8) >> put c
    put (UpdatePlayer p) = put (7 :: Word8) >> put p
    put (UpdateEntities e) = put (8 :: Word8) >> put e
    put (GetForegroundWindow) = put (9 :: Word8)
    put (SetForegroundWindow w) = put (10 :: Word8) >> put w
    put (GetGameWindow) = put (11 :: Word8)

    get = do (c :: Word8) <- get
             case c of
               1 -> do { s <- get; c <- get; return $ ChangeKeyState s c }
               2 -> do { s <- get; b <- get; return $ ChangeMouseState s b }
               3 -> do { x <- get; y <- get; return $ RelMoveMouse x y }
               4 -> do { x <- getFloat32be; y <- getFloat32be; return $ AbsMoveMousePerc x y }
               5 -> do { r <- getFloat32be; return $ OrientCamera r }
               6 -> do { c <- get; return $ UpdateCamera c }
               7 -> do { p <- get; return $ UpdatePlayer p }
               8 -> do { e <- get; return $ UpdateEntities e }
               9 -> return GetForegroundWindow
               10 -> do { w <- get; return $ SetForegroundWindow w }
               11 -> return GetGameWindow
               _ -> error "error reading command"

sendCommand :: (MonadIO m) => CommandChannel m -> Command -> m ()
sendCommand = ch_send

readCommand :: (MonadIO m) => CommandChannel m -> m Command
readCommand = ch_read

-- we use udp for bulk game state updates
createStateUpdatesChannel :: (MonadIO m) => Socket -> Maybe SockAddr -> m (CommandChannel m)
createStateUpdatesChannel sock addr =
    do counter <- newPacketCounter classify
       let ch = channelFromUDPSocket sock addr
       return $ CommandChannel
                  {
                    ch_send = \cmd -> {- liftIO (debugIO $ "send-c " ++ show cmd) >> -} sendCountedPacket ch 16384 cmd counter
                  , ch_read = readCountedPacket ch 16384 counter
                  }

-- we use tcp for small control events, assume socket is already connected
createGameControlChannel :: (MonadIO m) => Socket -> m (CommandChannel m)
createGameControlChannel sock  =
    do let ch = channelFromTCPSocket sock
       return $ CommandChannel
                  { 
                    ch_send = \cmd -> liftIO (debugIO $ "send-l " ++ show cmd) >> sendLengthEncodedPacket ch cmd
                  , ch_read = readLengthEncodedPacket ch
                  }
