{-# LANGUAGE FlexibleContexts #-}

module Common.Http where

import Data.Maybe
import Data.Either.Combinators (eitherToError)
import Control.Applicative
import Control.Monad.Catch (Exception, MonadCatch, MonadThrow, try)
import Control.Monad.Reader
import Control.Monad.Except
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B

import Network.HTTP.Client (Request)
import qualified Network.HTTP.Client as H
import qualified Network.HTTP.Client.TLS as T

import Common.Types

data Cred = Cred
    { username :: String
    , password :: String
    }
    deriving (Eq, Ord, Read, Show)

type Auth = Request -> Request

basicAuthM :: MonadReader Cred m => m Auth
basicAuthM = asks $ \c -> H.applyBasicAuth (B.pack $ username c) (B.pack $ password c)

basicAuth :: UserName -> Password -> Auth
basicAuth u p = H.applyBasicAuth (B.pack u) (B.pack p)

apiCall :: (Exception e, Functor m, MonadError e m, MonadCatch m, MonadIO m)
        => Maybe Auth -> Url -> m ByteString
apiCall mauth url = try (unsafeApiCall mauth url) >>= eitherToError

buildRequest :: (Functor m, MonadThrow m) => Maybe Auth -> Url -> m Request
buildRequest mauth url = fromMaybe id mauth <$> H.parseUrl url

unsafeApiCall :: (Functor m, MonadThrow m, MonadIO m)
              => Maybe Auth -> Url -> m ByteString
unsafeApiCall mauth url = do
    buildRequest mauth url >>= \req -> liftIO $ do
        H.withManager T.tlsManagerSettings $ \mgr -> do
            H.withResponse req mgr handle
    where handle = fmap B.concat . H.brConsume . H.responseBody
