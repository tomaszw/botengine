module GameState where

import Data.IORef
import Control.Concurrent
import Packet
import qualified Data.Map as M
import Data.Map (Map)

import Aion

----
---- Game State
----
data GameState = GameState { game_player :: IORef Player
                           , game_camera :: IORef Camera
                           , game_entities :: IORef [Entity]
                           , game_entity_map :: IORef (Map Int Entity)
                           , player_pack_cnt :: Counter
                           , camera_pack_cnt :: Counter
                           , entities_pack_cnt :: Counter }


updateGameState :: Packet -> GameState -> IO ()
updateGameState p s =
    case packet_data p of
      UpdateCamera c ->
          withCounter p (camera_pack_cnt s) $
          writeIORef (game_camera s) c
      UpdatePlayer ply ->
          withCounter p (player_pack_cnt s) $
          writeIORef (game_player s) ply
      UpdateEntityList e ->
          withCounter p (entities_pack_cnt s) $
          writeIORef (game_entities s) e >> writeIORef (game_entity_map s) (make_map e)
    where
      make_map es = M.fromList $ zip (map entity_id es) es

newGameState :: IO GameState
newGameState =
    do p <- newIORef undefined
       c <- newIORef undefined
       e <- newIORef undefined
       m <- newIORef undefined
       p_cnt <- newCounter
       c_cnt <- newCounter
       e_cnt <- newCounter
       return $ GameState { game_player = p
                          , game_camera = c
                          , game_entities = e
                          , game_entity_map = m
                          , player_pack_cnt = p_cnt
                          , camera_pack_cnt = c_cnt
                          , entities_pack_cnt = e_cnt }
