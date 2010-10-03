module GameState ( GameState(..)
                 , runUpdatesServer
                 , newGameState
                 ) where

import Network.Socket

import Common
import RemoteCommand
import Aion

----
---- mutable Game State
----
data GameState = GameState { game_player :: IORef Player
                           , game_camera :: IORef Camera
                           , game_entities :: IORef [Entity] }

-- run update command against game state
updateGameState :: (MonadIO m) => Command -> GameState -> m ()
updateGameState cmd s =
    liftIO $ update cmd
    where
      update ( UpdateCamera c ) = writeIORef (game_camera s) c
      update ( UpdatePlayer p ) = writeIORef (game_player s) p
      update ( UpdateEntities e ) = writeIORef (game_entities s) e
      update _ = hPutStrLn stderr $ "unhandled update command: " ++ show cmd

-- run state update server
runUpdatesServer :: (MonadIO m) => PortNumber -> GameState -> m ()
runUpdatesServer port state =
    liftIO $
           do sock <- socket AF_INET Datagram defaultProtocol
              bindSocket sock ( SockAddrInet port iNADDR_ANY )
              chann <- createStateUpdatesChannel sock Nothing
              parse chann
    where
      parse ch =
          do c <- readCommand ch
             updateGameState c state
             parse ch

newGameState :: IO GameState
newGameState =
    do p <- newIORef undefined
       c <- newIORef undefined
       e <- newIORef undefined
       return $ GameState { game_player = p
                          , game_camera = c
                          , game_entities = e }
