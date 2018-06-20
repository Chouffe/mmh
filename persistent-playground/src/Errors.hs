module Errors where

import           Control.Exception          (SomeException, handle)
import           Data.ByteString.Lazy.Char8 (pack)
import           Servant.Server             (ServantErr (..), err500)

runWithServantHandler :: IO a -> IO (Either ServantErr a)
runWithServantHandler action = handle servantErrHandler (action >>= (return . Right))

servantErrHandler :: SomeException -> IO (Either ServantErr a)
servantErrHandler e = return $ Left $ err500 { errBody = pack (show e)}
