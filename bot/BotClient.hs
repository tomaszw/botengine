module BotClient ( connectBot ) where

import Control.Monad
import Control.Monad.Trans
import Network.Socket
import System.IO
import Comm
import Aion

port = 5555

connectBot :: (MonadIO m) => String -> m (Channel m)
connectBot addr = liftIO $ do
       sock <- socket AF_INET Stream defaultProtocol
       a <- inet_addr addr
       connect sock $ SockAddrInet port a
       return $ channelFromSocket sock
