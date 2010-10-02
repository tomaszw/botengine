{-# LANGUAGE RankNTypes, ExistentialQuantification,MultiParamTypeClasses,FlexibleInstances #-}
module Main where

import Control.Monad
import Control.Monad.Trans
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import Data.Word

class FooClass a where
    zonk :: (MonadIO m) => a -> m Int


data Foobar m = Foobar { bla :: Int, unfoobar :: m (), kabzoom :: m () }

instance (MonadIO m) => FooClass (Foobar m) where
    zonk (Foobar bla m n ) = frillock bla

frillock :: (MonadIO m) => Int -> m Int
frillock i = return $ i*2

main = putStrLn "bla"

class Process m a where
    processGetModuleHandle :: a -> String -> m Word32

data Channel m = Channel { channSend :: ByteString -> m Int 
                         , channRecv :: Int -> m ByteString }

instance (MonadIO m) => Process m (Channel m) where
    processGetModuleHandle c name = getModuleHandle c name

getModuleHandle :: (MonadIO m) => Channel m -> String -> m Word32
getModuleHandle c name = return 0
