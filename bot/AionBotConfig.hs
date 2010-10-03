module AionBotConfig
    ( AionBotConfig (..)
    , defaultConfig
    , Rotation (..)
    , RotationElem (..)
    ) where

import Keys
import RemoteCommand

data AionBotConfig = AionBotConfig
    {
      combat_rotation :: Rotation
    , loot_key :: KeyCode
    , threshold_grind_upper_level :: Int
    , threshold_grind_lower_level :: Int
    }

defaultConfig :: AionBotConfig
defaultConfig =
    AionBotConfig
    {
      combat_rotation = Repeat [ qb 2, Delay 0.1
                               , qb 3, Delay 0.1
                               , qb 4, Delay 0.1
                               , qb 5, Delay 0.1
                               , qb 6, Delay 0.1
                               , aqb 2, Delay 0.1
                               , aqb 5, Delay 0.1 ]
    , loot_key = keyQuickbarBase + 0
    -- grind mob this number of levels higher/lower
    , threshold_grind_upper_level = 1
    , threshold_grind_lower_level = 5
    }
    where
      -- quickbar
      qb i  = KeyPress (keyQuickbarBase + i)
      -- alternate quickbar
      aqb i = Rotation $ Once [KeyHold keyLeftAlt, Delay 0.01, qb i, Delay 0.01, KeyRelease keyLeftAlt, Delay 0.01]
      
data Rotation   = Repeat [RotationElem]
                | Once [RotationElem]
                deriving ( Eq, Show )
data RotationElem = KeyPress KeyCode
                  | KeyHold KeyCode
                  | KeyRelease KeyCode
                  | Delay Float
                  | Rotation Rotation -- nested rotation
                  deriving ( Eq, Show )
