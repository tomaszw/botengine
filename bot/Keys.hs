module Keys where

import Data.Char
import Data.Word

charKey :: Char -> Word32
charKey = fromIntegral . ord

keyReturn = 0x0D :: Word32
keyLeftShift = 0xA0 :: Word32
keyRightShift = 0xA1 :: Word32
keyLeftAlt = 0xA4 :: Word32
keyRightAlt = 0xA5 :: Word32
keyLeftCtrl = 0xA2 :: Word32
keyRightCtrl = 0xA3 :: Word32


keyForward = charKey 'W'
keyBackward = charKey 'S'
keyTab = 0x09 :: Word32
keyNextTarget = keyTab
keyJump = charKey ' '
keyStrafeLeft = charKey 'Q'
keyStrafeRight = charKey 'E'
keyQuickbarBase = charKey '0'
keyAttack = keyQuickbarBase + 1

