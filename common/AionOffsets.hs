module AionOffsets where

import Data.Word

playerHPOffset, playerMaxHPOffset, playerManaOffset
 , playerMaxManaOffset, playerCombatOffset, playerLevelOffset
 , playerXOffset, playerYOffset, playerZOffset, playerRotOffset
 , playerCastingOffset, playerCubeCurrentOffset, playerCubeMaxOffset
 , playerXPToGoOffset, playerXPOffset, playerIDOffset, playerNameOffset
 
 , nameOffset, ownerOffset, hpOffset, typeOffset
 , type2Offset, idOffset, stateOffset, levelOffset
 , posXOffset, posYOffset, posZOffset
 , infoAddressOffset, targetIDOffset
 , entityListOffset, targetBaseOffset
  :: Word32

playerHPOffset = playerMaxHPOffset + 0x4
playerMaxHPOffset = 0xB04AB4
playerManaOffset = playerMaxManaOffset + 0x4
playerMaxManaOffset = playerHPOffset + 0x4
playerCombatOffset = 0x4F887C
playerLevelOffset = 0xB04A90
playerXOffset = playerYOffset + 0x4
playerYOffset = 0xAF9C40
playerZOffset = playerXOffset + 0x4
playerRotOffset = 0xAF9880
playerCastingOffset = 0xAFA4D0
playerCubeCurrentOffset = playerCubeMaxOffset + 0x4
playerCubeMaxOffset = 0xB04B0C
playerXPToGoOffset = playerXPOffset-0x10
playerXPOffset = 0xB04AA8
playerIDOffset = 0xAF9C30
playerNameOffset = 0xB3D4E8

nameOffset = 0x3A
ownerOffset = 0xC0
hpOffset = 0x38
typeOffset = 0x18
type2Offset = 0x1A8
idOffset = 0x24
stateOffset = 0x258
levelOffset = 0x36
posXOffset = 0x30
posYOffset = 0x2C
posZOffset = 0x34
infoAddressOffset = 0x1d0
targetIDOffset = 0x2D4

entityListOffset = 0xB04D78
targetBaseOffset = 0x6E6D80
