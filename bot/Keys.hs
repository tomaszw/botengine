module Keys where

import Data.Char
import Data.Word

charKey :: Char -> Word32
charKey = fromIntegral . ord

keyForward = charKey 'W'
keyBackward = charKey 'S'
keyTab = 0x09 :: Word32
keyNextTarget = keyTab