module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import qualified Control.Exception as E
import qualified Data.ByteString.Lazy as B
import Network.Socket
import System.IO
import Data.Binary
import Foreign.C.String
import Foreign.C.Types
import Foreign
import Graphics.Win32 ( HWND )
import qualified Graphics.Win32 as Win32

import Comm
import Aion
import Process
import WinProcess

foreign import ccall "find_window" c_find_window :: CString -> IO HWND
foreign import ccall "mouse_set_cursor_pos_perc" c_mouse_set_cursor_pos_perc :: HWND -> CDouble -> CDouble -> IO ()
foreign import ccall "key_down" c_key_down :: CInt -> IO ()
foreign import ccall "key_up" c_key_up :: CInt -> IO ()
foreign import ccall "mousebutton_down" c_mousebutton_down :: CInt -> IO ()
foreign import ccall "mousebutton_up" c_mousebutton_up :: CInt -> IO ()
foreign import ccall "mouse_move" c_mouse_move :: CInt -> CInt -> IO ()
foreign import stdcall "SetForegroundWindow" winsetForegroundWindow :: HWND -> IO ()

port = 5555

len32 :: B.ByteString -> Word32
len32 s = fromIntegral (B.length s)

ptrToWord32 :: Ptr a -> Word32
ptrToWord32 p = fromIntegral $ p `minusPtr` nullPtr

word32ToPtr :: Word32 -> Ptr a
word32ToPtr w = nullPtr `plusPtr` (fromIntegral w)

conversation :: WinProcess -> HWND -> Channel IO -> IO ()
conversation p hwnd c = 
    do code <- channRecvBinary' c 1
       let cmd = commFromCode code
       handle cmd
       conversation p hwnd c
    where
      handle SetMousePos =
          do x <- channRecvBinary' c 4 :: IO Word32
             y <- channRecvBinary' c 4 :: IO Word32
             let x' = fromIntegral x / 100.0
                 y' = fromIntegral y / 100.0
             c_mouse_set_cursor_pos_perc hwnd x' y'
             return ()
      handle SendKey =
          do state <- channRecvBinary' c 1 :: IO KeyState
             key <- channRecvBinary' c 4 :: IO KeyCode
             case state of
               Down -> c_key_down $ fromIntegral key
               Up -> c_key_up $ fromIntegral key
      handle SendMouse =
          do state <- channRecvBinary' c 1
             btn <- channRecvBinary' c 1
             case (state,btn) of
               (Down,L) -> c_mousebutton_down 0
               (Up,L) -> c_mousebutton_up 0
               (Down,R) -> c_mousebutton_down 1
               (Up,R) -> c_mousebutton_up 1
      handle SendMouseMove =
          do x <- channRecvBinary' c 4 :: IO Word32
             y <- channRecvBinary' c 4 :: IO Word32
             c_mouse_move (fromIntegral x) (fromIntegral y)
      handle GetForegroundWindow =
          do h <- Win32.getForegroundWindow
             channSendBinary c ( ptrToWord32 h )
      handle SetForegroundWindow =
          do ptr <- channRecvBinary' c 4
             let h = word32ToPtr ptr
             winsetForegroundWindow h
      handle GetGameWindow =
          do channSendA c ( encode $ ptrToWord32 hwnd )

      handle GetCamera =
          do cam <- encode <$> getCamera p
             channSendA c (encode $ len32 cam)
             channSendA c cam
      handle GetPlayer =
          do ply <- encode <$> getPlayerData p
             channSendA c (encode $ len32 ply)
             channSendA c ply
      handle GetEntityList =
          do ents <- getEntityList p
             channSendA c (encode $ (fromIntegral (length ents) :: Word32))
             mapM_ sendEnt ents
             where
               sendEnt e = do let ent = encode e
                              channSendA c (encode $ len32 ent)
                              channSendA c ent

      handle c = error $ "unknown command " ++ show c
    
server :: WinProcess -> HWND -> IO ()
server p w =
    do sock <- socket AF_INET Stream defaultProtocol
       bindSocket sock (SockAddrInet port iNADDR_ANY)
       listen sock 1
       putStrLn "listening.."
       loop sock
    where
      loop sock = do (client,addr) <- accept sock
                     putStrLn $ "client connected from " ++ show addr
                     conversation p w (channelFromSocket client) `E.catch` err client
                     disconnect
          where
            err :: Socket -> E.SomeException -> IO ()
            err c e = putStrLn (show e) >> (E.try (sClose c) :: IO (Either E.SomeException ())) >> disconnect
            disconnect = putStrLn "client disconnected." >> loop sock

windowName = "AION Client"

main :: IO ()
main = do hSetBuffering stdout LineBuffering
          p <- openGameProcess windowName
          w <- withCString windowName $ \s -> c_find_window s
          withSocketsDo (server p w)
          return ()
