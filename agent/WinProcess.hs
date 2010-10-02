module WinProcess ( WinProcess, openGameProcess ) where

import Data.Word
import Control.Monad
import Control.Monad.Trans

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import Data.ByteString.Internal ( create, createAndTrim, fromForeignPtr )
import qualified System.Win32.DebugApi as Win32
import qualified System.Win32 as Win32

import Foreign
import Foreign.C
import Foreign.C.String

import Process

newtype WinProcess = WinProcess Win32.PHANDLE

foreign import ccall "find_module_by_name" c_find_module_by_name :: Ptr () -> CString -> IO Word32
foreign import ccall "open_process_by_window_name" c_open_process_by_window_name :: CString -> IO ( Ptr () )
foreign import ccall "read_process_memory" c_read_process_memory :: Ptr () -> Word32 -> Word32 -> Ptr Word8 -> IO ()

openGameProcess :: String -> IO WinProcess
openGameProcess winname =
    withCString winname $ \winnameS ->
        do ptr <- c_open_process_by_window_name winnameS
           return $ WinProcess ptr

instance (MonadIO m) => Process m WinProcess where
    processGetModuleHandle (WinProcess p) name = liftIO $
        withCString name $ \nameS -> c_find_module_by_name p nameS
    processReadMemory (WinProcess p) addr len =
        liftIO $
               do str <- create len $ \ptr -> c_read_process_memory p addr (fromIntegral len) ptr
                  return $ B.fromChunks [str]
           