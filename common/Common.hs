module Common ( module Data.Ord
              , module Data.List
              , module Data.List.Split
              , module Data.Int
              , module Data.Word
              , module Data.Bits
              , module Data.IORef
              , module Control.Concurrent
              , module Control.Monad
              , module Control.Monad.Trans
              , module Control.Applicative
              , module System.IO
              , debugIO
              ) where

import Data.List
import Data.List.Split
import Data.Word
import Data.Int
import Data.Bits
import Data.Ord
import Data.IORef
import Control.Concurrent
import Control.Monad
import Control.Monad.Trans
import Control.Applicative
import System.IO
import Data.Time
import Data.Time.Clock

debugIO :: String -> IO ()
debugIO s =
    do t <- getCurrentTime
       hPutStrLn stderr $ (show t) ++ ": " ++ s
       hFlush stderr

