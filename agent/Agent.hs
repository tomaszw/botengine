module Main where

import Common
import qualified Control.Exception as E
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Time
import Data.Time.Clock
import Network.Socket
import Data.Binary
import Foreign.C.String
import Foreign.C.Types
import Foreign
import Graphics.Win32 ( HWND )
import qualified Graphics.Win32 as Win32

import Aion
import Process
import WinProcess
import RemoteCommand

foreign import ccall "find_window" c_find_window :: CString -> IO HWND
foreign import ccall "mouse_set_cursor_pos_perc" c_mouse_set_cursor_pos_perc :: HWND -> CDouble -> CDouble -> IO ()
foreign import ccall "key_down" c_key_down :: CInt -> IO ()
foreign import ccall "key_up" c_key_up :: CInt -> IO ()
foreign import ccall "mousebutton_down" c_mousebutton_down :: CInt -> IO ()
foreign import ccall "mousebutton_up" c_mousebutton_up :: CInt -> IO ()
foreign import ccall "mouse_move" c_mouse_move :: CInt -> CInt -> IO ()
foreign import stdcall "SetForegroundWindow" winsetForegroundWindow :: HWND -> IO ()

port :: PortNumber
port = 5555

windowName :: String
windowName = "AION Client"

main :: IO ()
main = do hSetBuffering stdout LineBuffering
          p <- openGameProcess windowName
          w <- withCString windowName $ \s -> c_find_window s
          withSocketsDo $
            runCommandServer p w
          return ()

runCommandServer :: WinProcess -> HWND -> IO ()
runCommandServer p w =
    do sock <- socket AF_INET Stream defaultProtocol
       bindSocket sock (SockAddrInet port iNADDR_ANY)
       listen sock 1
       loop sock
    where
      talk (client,addr) =
          do ch <- createGameControlChannel client
             orienter <- newEmptyMVar
             conversation p w ch (ConversationState orienter)

      loop sock =
          do putStrLn $ "listening..."
             (client,addr) <- accept sock
             let updates_addr | SockAddrInet _ host_addr <- addr = SockAddrInet port host_addr
                              | otherwise                        = error "unexpected address"
             updates <- forkIO $ runStateUpdatesSender updates_addr p 20
             handle (client,addr)
             killThread updates

             loop sock

      handle (client,addr) =
          ( do putStrLn $ "incoming connection from " ++ show addr
               talk (client,addr)
               disconnect client
          ) `E.catch` ( \(err::E.SomeException) ->
                            do putStrLn $ "connection error: " ++ show err
                               disconnect client
                      )
          where
            disconnect sock =
                do E.try (sClose sock) :: IO (Either E.SomeException ())
                   putStrLn "client disconnected."

runStateUpdatesSender :: SockAddr -> WinProcess -> Int -> IO ()
runStateUpdatesSender addr p delay_ms =
    do sock <- socket AF_INET Datagram defaultProtocol
       ch <-createStateUpdatesChannel sock (Just addr)
       putStrLn $ "starting state notifications to remote " ++ show addr
       updates ch 0
    where
      updates ch i =
          do camera <- getCamera p
             sendCommand ch $ UpdateCamera camera
             when big_update $
                  do ply <- getPlayerData p
                     ent <- getEntityList p
                     sendCommand ch $ UpdatePlayer ply
                     sendCommand ch $ UpdateEntities ent
                     putStr "."
                     hFlush stdout
             threadDelay $ delay_ms * 1000
             updates ch (i+1)
          where
            big_update = i `mod` 10 == 0

data ConversationState = ConversationState { cameraOrienter :: MVar ThreadId }

conversation :: WinProcess -> HWND -> CommandChannel IO -> ConversationState -> IO ()
conversation p hwnd ch state = 
    do cmd <- readCommand ch
       handle cmd
       conversation p hwnd ch state
    where
      handle (AbsMoveMousePerc x y)    = withActiveWindow hwnd $ c_mouse_set_cursor_pos_perc hwnd (realToFrac x) (realToFrac y)
      handle (RelMoveMouse dx dy)      = withActiveWindow hwnd $ c_mouse_move (fromIntegral dx) (fromIntegral dy)
      handle (ChangeKeyState Down key) = withActiveWindow hwnd $ c_key_down $ fromIntegral key
      handle (ChangeKeyState Up key)   = withActiveWindow hwnd $ c_key_up $ fromIntegral key
      handle (ChangeMouseState Down L) = withActiveWindow hwnd $ c_mousebutton_down 0
      handle (ChangeMouseState Up   L) = withActiveWindow hwnd $ c_mousebutton_up 0
      handle (ChangeMouseState Down R) = withActiveWindow hwnd $ c_mousebutton_down 1
      handle (ChangeMouseState Up   R) = withActiveWindow hwnd $ c_mousebutton_up 1
      handle (OrientCamera t_r)        = withActiveWindow hwnd $ orientCamera p hwnd t_r (cameraOrienter state)
      handle c = error $ "unhandled command " ++ show c

withActiveWindow :: HWND -> IO a -> IO a
withActiveWindow hwnd act =
    do winsetForegroundWindow hwnd
       threadDelay ( 10 * 10^3 )
       act

orientCamera :: WinProcess -> HWND -> Float -> MVar ThreadId -> IO ()
orientCamera p hwnd t_r orienter =
    do maybe_id <- tryTakeMVar orienter
       case maybe_id of
         Just id -> killThread id
         Nothing -> return ()
       t0 <- getCurrentTime
       id <- forkIO (acquire >> runOrienter t0)
       putMVar orienter id

    where
      -- only let orienter run for few seconds
      runOrienter :: UTCTime -> IO ()
      runOrienter t0 =
          do t1 <- getCurrentTime
             let diff = realToFrac (diffUTCTime t1 t0) :: Float
             case () of
               _ | diff <= 3 -> orientCameraStep p t_r >> delay 20 >> runOrienter t0
                 | otherwise -> release
      delay ms  = threadDelay $ ms * 10^3
      acquire   = c_mouse_set_cursor_pos_perc hwnd 50 50 >> delay 50 >> c_mousebutton_down 1 >> delay 50
      release   = withActiveWindow hwnd $ c_mousebutton_up 1 >> delay 50

orientCameraStep :: WinProcess -> Float -> IO ()
orientCameraStep p t_r =
    do camera <- getCamera p
       let r0 = camera_rot camera
       rotate r0 t_r
    where
      epsilon = 2
      rotate r0 t_r
          | abs (t_r - r0) < epsilon = return () -- hooray rotated already
          | otherwise =
              do let diff_a = max t_r r0 - min t_r r0
                     diff_b = 360 - diff_a
                     d =
                         case () of
                           _ | r0 >= t_r, diff_a <= diff_b -> -diff_a
                             | r0 >= t_r, diff_a >  diff_b ->  diff_b
                             | r0 <  t_r, diff_a <= diff_b ->  diff_a
                             | otherwise                   -> -diff_b
                     speed = min 25 . max 2 $ (abs d / 5) ** 1.5 -- nice smooth function for rotation speed; lower when nearing target rotation
                     dx = speed * (- (signum d))
                 c_mouse_move (round dx) 0

{-
-- rotate camera by given amount
rotateCamera :: Float -> AionBot ()
rotateCamera delta =
    -- shouldn't be taking more than few secs
    do timeout 2.0 $
               do parkMouse
                  sendMouseBtn C.Down C.R
                  finally (sendMouseBtn C.Up C.R) $ do
                    delay 0.15
                    c <- getCamera
                    -- target rotation angle
                    let t_r = snap $ camera_rot c + delta
                    rotate (camera_rot c) t_r
       delay 0.1
       return ()
    where
      epsilon = 2
      rotate r0 t_r
          | abs (t_r - r0) < epsilon = return () -- hooray rotated
          | otherwise =
              do let diff_a = max t_r r0 - min t_r r0
                     diff_b = 360 - diff_a
                     d =
                         case () of
                           _ | r0 >= t_r, diff_a <= diff_b -> -diff_a
                             | r0 >= t_r, diff_a >  diff_b ->  diff_b
                             | r0 <  t_r, diff_a <= diff_b ->  diff_a
                             | otherwise                   -> -diff_b
                     speed = min 20 . max 2 $ (abs d / 5) ** 1.5 -- nice smooth function for rotation speed; lower when nearing target rotation
                     dx = speed * (- (signum d))
                 -- debug $ printf "tr=%f r=%f dx=%f d=%f" t_r r0 dx d
                 sendMouseMoveFast (round dx) 0
                 -- delay and fetch new camera orientation from game, then continue with rotation
                 delay 0.1
                 c <- getCamera
                 rotate (camera_rot c) t_r
-}

