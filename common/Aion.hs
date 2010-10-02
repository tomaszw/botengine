module Aion ( Player(..),Entity(..),EntityType(..),Camera(..)
            , getEntityList
            , getPlayerData
            , getCamera ) where

import AionOffsets
import Math
import Data.Binary
import Data.Binary.Get
import Data.Binary.IEEE754
import Data.Int
import Data.Char
import Data.Bits
import qualified Data.ByteString.Lazy as B
import Process

data Camera = Camera {
      camera_rot :: !Float
    } deriving (Show)

instance Binary Camera where
    put c =
        put (camera_rot c)
    get =
        do rot <- get
           return $ Camera rot

data Player = Player {
      player_id :: !Int
    , player_hp :: !Int
    , player_hpmax :: !Int
    , player_level :: !Int
    , player_pos :: !Vec3
    , player_rot :: !Float
    } deriving (Show)

instance Binary Vec3 where
    put (Vec3 x y z) = put x >> put y >> put z
    get = do { x <- get; y <- get; z <- get; return $ Vec3 x y z }
           

instance Binary Player where
    put p =
        do put $ player_id p
           put $ player_hp p
           put $ player_hpmax p
           put $ player_level p
           put $ player_pos p
           put $ player_rot p
    get =
        do id <- get
           hp <- get
           hpmax <- get
           level <- get
           pos <- get
           rot <- get
           return $ Player { player_id = id, player_hp = hp, player_hpmax = hpmax, player_level = level
                           , player_pos = pos, player_rot = rot }

data EntityType = EPlayer | ENPC | EOther deriving (Eq, Show)

instance Binary EntityType where
    put EPlayer = put (0 :: Word8)
    put ENPC = put (1 :: Word8)
    put EOther = put (2 :: Word8)
    get =
        do v <- get :: Get Word8
           return $ case v of
             0 -> EPlayer
             1 -> ENPC
             2 -> EOther
             _ -> error "invalid binary EntityType value"
    
mkEntityType :: Int -> EntityType
mkEntityType 1 = EPlayer
mkEntityType 2 = ENPC
mkEntityType _ = EOther

data Entity = Entity {
      entity_addr :: !Word32
    , entity_id :: !Int
    , entity_target_id :: !Int
    , entity_hp :: !Int
    , entity_level :: !Int
    , entity_pos :: !Vec3
    , entity_type :: !EntityType
    , entity_type2 :: !Int
    , entity_name :: String
    } deriving (Eq, Show)

instance Binary Entity where
    put e = put (entity_addr e) >> put (entity_id e) >> put (entity_target_id e)
            >> put (entity_hp e) >> put (entity_level e) >> put (entity_pos e)
            >> put (entity_type e) >> put (entity_type2 e) >> put (entity_name e)
    get =
        do { addr <- get; id <- get; target_id <- get; hp <- get; level <- get
           ; pos <- get; typ <- get; typ2 <- get; name <- get
           ; return $ Entity { entity_addr = addr, entity_id = id, entity_target_id = target_id
                             , entity_hp = hp, entity_level = level, entity_pos = pos
                             , entity_type = typ, entity_type2 = typ2, entity_name = name }
           }
                      
peekInt :: (Process m p) => p -> Word32 -> m Int
peekInt c offset =
    do b <- processReadMemory c offset 4
       return . fromIntegral $ runGet getWord32le b

peekWord32 :: (Process m p) => p -> Word32 -> m Word32
peekWord32 c offset =
    do b <- processReadMemory c offset 4
       return $ runGet getWord32le b

peekFloat :: (Process m p) => p -> Word32 -> m Float
peekFloat c offset =
    do b <- processReadMemory c offset 4
       return $ runGet getFloat32le b

getUniString :: (Process m p) => p -> Word32 -> m String
getUniString c addr =
    do b <- processReadMemory c addr 64
       return $ decode b
    where
      decode b | B.length b < 2 = []
               | B.head b == 0 || B.head b > 127 = []
               | otherwise      = chr (fromIntegral $ B.head b) : decode (B.drop 2 b)

getEntity :: (Process m p) => p -> Word32 -> m Entity
getEntity c addr =
    do info_addr <- peekWord32 c ( addr + infoAddressOffset )
       x <- peekFloat c ( addr + posXOffset )
       y <- peekFloat c ( addr + posYOffset )
       z <- peekFloat c ( addr + posZOffset )
       typ <- peekInt c ( info_addr + typeOffset )
       id <- peekInt c ( info_addr + idOffset )
       level <- peekInt c ( info_addr + levelOffset )
       hp <- peekInt c ( info_addr + hpOffset )
       type2 <- peekInt c ( info_addr + type2Offset )
       state <- peekInt c ( info_addr + stateOffset )
       target_id <- peekInt c ( info_addr + targetIDOffset )
       name <- getUniString c ( info_addr + nameOffset )
       return $ Entity {
                    entity_addr = addr
                  , entity_id = id
                  , entity_target_id = target_id
                  , entity_hp = hp .&. 0xFF -- is in percent
                  , entity_level = level .&. 0xFF
                  , entity_pos = Vec3 x y z
                  , entity_type = mkEntityType typ
                  , entity_type2 = type2
                  , entity_name = name
                  }

getEntityList :: (Process m p) => p -> m [Entity]
getEntityList c =
    do game_dll <- processGetModuleHandle c "Game.dll"
       addr_list <- peekWord32 c (game_dll + entityListOffset)
       array_addr <- peekWord32 c (addr_list + 0x48)
       node_addr <- peekWord32 c array_addr
       getEntityList' c node_addr

getEntityList' c node =
    do entity_addr <- peekWord32 c (node + 0x0c)
       case good_ptr entity_addr of
         False -> return []
         True  ->
             do e <- getEntity c entity_addr
                case () of
                  _ | entity_name e /= "" ->
                        do es <- next
                           return (e:es)
                    | otherwise -> next
  where
    stop = 0xCDCDCDCD
    good_ptr p = p /= 0 && p /= stop
    next = do p <- peekWord32 c node
              if good_ptr p
                then getEntityList' c p
                else return []

getPlayerData :: (Process m p) => p -> m Player
getPlayerData c =
    do game_dll <- processGetModuleHandle c "Game.dll"
       player_id_ <- peekInt c (game_dll + playerIDOffset)
       player_hp_ <- peekInt c (game_dll + playerHPOffset)
       player_hpmax_ <- peekInt c (game_dll + playerMaxHPOffset)
       player_level_ <- peekInt c (game_dll + playerLevelOffset)
       player_x_ <- peekFloat c (game_dll + playerXOffset)
       player_y_ <- peekFloat c (game_dll + playerYOffset)
       player_z_ <- peekFloat c (game_dll + playerZOffset)
       player_rot_ <- peekFloat c (game_dll + playerRotOffset)
       return $ Player { player_id = player_id_
                       , player_hp = player_hp_
                       , player_hpmax = player_hpmax_
                       , player_level = player_level_ .&. 0xFF
                       , player_pos = Vec3 player_x_ player_y_ player_z_
                       , player_rot = player_rot_
                       }

getCamera :: (Process m p) => p -> m Camera
getCamera c =
    do game_dll <- processGetModuleHandle c "Game.dll"
       rot <- peekFloat c (game_dll + playerRotOffset)
       return $ Camera { camera_rot = rot }
