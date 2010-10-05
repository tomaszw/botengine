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
    , heal_self_rotation :: Rotation
    , loot_key :: KeyCode
    , threshold_grind_upper_level :: Int
    , threshold_grind_lower_level :: Int
    , safe_margin :: Float
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
    , heal_self_rotation = Once [ qb 8, Delay 5.0
                                , qb 9 ]
    , loot_key = keyQuickbarBase + 0
    -- grind mob this number of levels higher/lower
    , threshold_grind_upper_level = 1
    , threshold_grind_lower_level = 5
    , safe_margin = 2
    }
    where
      -- quickbar
      qb i  = KeyPress (keyQuickbarBase + i)
      -- alternate quickbar
      aqb i = HoldModKeyPress keyLeftAlt (keyQuickbarBase + i)
      
data Rotation   = Repeat [RotationElem]
                | Once [RotationElem]
                deriving ( Eq, Show )
data RotationElem = KeyPress KeyCode
                  | HoldModKeyPress KeyCode KeyCode
                  | Delay Float
                  | Rotation Rotation -- nested rotation
                  deriving ( Eq, Show )
