module Process ( Process (..) ) where

import Data.Word
import Control.Monad
import Data.ByteString.Lazy (ByteString)

class (Monad m) => Process m a where
    processGetModuleHandle :: a -> String -> m Word32
    processReadMemory :: a -> Word32 -> Int -> m ByteString

