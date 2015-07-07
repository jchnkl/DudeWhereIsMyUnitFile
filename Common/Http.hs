{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Common.Http where

import Data.Maybe
import Control.Applicative
import Control.Monad.Catch (MonadCatch(..))
import Control.Monad.Except
-- import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B

import Network.HTTP.Client (Request)
import Control.Exception (SomeException)
import qualified Network.HTTP.Client as H
import qualified Network.HTTP.Client.TLS as T

import Common.Types

type Auth = Request -> Request

basicAuth :: UserName -> Password -> Auth
basicAuth u p = H.applyBasicAuth (B.pack u) (B.pack p)

apiCall :: (MonadError String m, MonadIO m) => Maybe Auth -> Url -> m ByteString
apiCall = undefined
-- apiCall mauth url = liftIO $ flip catch undefined $ do
--     -- sendRequest = <$> H.parseUrl (obsApiUrl ++ searchUrl)
--
--     fromMaybe id mauth <$> H.parseUrl url >>= \req -> do
--         H.withManager T.tlsManagerSettings $ \mgr -> do
--             H.withResponse req mgr handleResponse

    where
    -- useAuth = case mauth of
    --     Nothing -> id
    --     Just au -> au
    handleResponse = fmap B.concat . H.brConsume . H.responseBody

-- -- handleException :: MonadError String m => SomeException -> m ByteString
-- handleException :: MonadError String m => SomeException -> m a
-- handleException (e::SomeException) = throwError _ -- . show $ (e::SomeException)
