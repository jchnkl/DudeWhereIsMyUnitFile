{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Arch where

import Data.Word
import Control.Monad
import Control.Applicative
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import qualified Data.ByteString as B

import Network.HTTP.Client (Request)
import qualified Network.HTTP.Client as H
import qualified Network.HTTP.Client.TLS as T
-- import Text.XML.Light.Types
-- import qualified Text.XML.Light as X

import qualified Data.Text as T
import Data.Aeson
import Data.Aeson.TH

import Data.Time.Clock (UTCTime)

import Common.Types

data Repository = Core
                | Extra
                | Testing
                | Multilib
                | MultilibTesting
                | Community
                | CommunityTesting
    deriving (Eq, Ord)

instance ToJSON Repository where
    toJSON = \case
        Core             -> "core"
        Extra            -> "extra"
        Testing          -> "testing"
        Multilib         -> "multilib"
        MultilibTesting  -> "multilib-testing"
        Community        -> "community"
        CommunityTesting -> "community-testing"

instance FromJSON Repository where
    parseJSON (String str) | str == "core"              = return Core
                           | str == "extra"             = return Extra
                           | str == "testing"           = return Testing
                           | str == "multilib"          = return Multilib
                           | str == "multilib-testing"  = return MultilibTesting
                           | str == "community"         = return Community
                           | str == "community-testing" = return CommunityTesting
                           | otherwise                  = mzero
    parseJSON _ = mzero

instance Show Repository where
    show r = let (String s) = toJSON r in T.unpack s

instance Read Repository where
    readsPrec _ r = case fromJSON (String $ T.pack r) of
        Error _   -> mzero
        Success a -> return a

data Architecture = Any | I686 | X86_64
    deriving (Eq, Ord)

instance ToJSON Architecture where
    toJSON = \case
        Any    -> "any"
        I686   -> "i686"
        X86_64 -> "x86_64"

instance FromJSON Architecture where
    parseJSON (String str) | str == "any"    = return Any
                           | str == "i686"   = return I686
                           | str == "x86_64" = return X86_64
    parseJSON _ = mzero

instance Show Architecture where
    show r = let (String s) = toJSON r in T.unpack s

instance Read Architecture where
    readsPrec _ r = case fromJSON (String $ T.pack r) of
        Error _   -> mzero
        Success a -> return a

data Flagged = IsFlagged | NotFlagged
    deriving (Eq, Ord, Read)

instance Show Flagged where
    show f = case f of
        IsFlagged -> "Flagged"
        NotFlagged -> "Not+Flagged"

data Param = Repository Repository
           | Architecture Architecture
           | Maintainer String
           | Packager String
           | Flagged Flagged
    deriving (Eq, Ord, Read, Show)

showParam :: Param -> String
showParam p = case p of
    Repository o   -> "repo=" ++ show o
    Architecture o -> "arch=" ++ show o
    Maintainer o   -> "maintainer=" ++ show o
    Packager o     -> "packager=" ++ show o
    Flagged o      -> "flagged=" ++ show o

data Search = Name String [Param]
            | Exact String [Param]
            | Description String [Param]
    deriving (Eq, Ord, Read, Show)

data Pkg = Pkg
    { pkg_pkgdesc         :: String
    , pkg_depends         :: [String]
    , pkg_licenses        :: [String]
    , pkg_last_update     :: UTCTime
    , pkg_build_date      :: UTCTime
    , pkg_compressed_size :: Word
    , pkg_installed_size  :: Word
    , pkg_filename        :: String
    , pkg_epoch           :: Word
    , pkg_provides        :: [String]
    , pkg_repo            :: Repository
    , pkg_maintainers     :: [String]
    , pkg_groups          :: [String]
    , pkg_conflicts       :: [String]
    , pkg_packager        :: String
    , pkg_arch            :: Architecture
    , pkg_pkgver          :: String
    , pkg_replaces        :: [String]
    , pkg_pkgname         :: String
    , pkg_url             :: String
    , pkg_pkgbase         :: String
    , pkg_pkgrel          :: String
    , pkg_flag_date       :: Maybe UTCTime
    }
    deriving (Eq, Ord, Read, Show)

$(deriveJSON defaultOptions { fieldLabelModifier = drop 4 } ''Pkg)

data SearchReply = SearchReply
    { searchReply_version :: Int
    , searchReply_limit   :: Int
    , searchReply_valid   :: Bool
    , searchReply_results :: [Pkg]
    }
    deriving (Eq, Ord, Read, Show)

$(deriveJSON defaultOptions { fieldLabelModifier = drop 12 } ''SearchReply)

data FilesReply = FilesReply
    { filesReply_repo              :: Repository
    , filesReply_dir_count         :: Word
    , filesReply_pkgname           :: String
    , filesReply_files_last_update :: UTCTime
    , filesReply_pkg_last_update   :: UTCTime
    , filesReply_arch              :: Architecture
    , filesReply_files             :: [FilePath]
    , filesReply_files_count       :: Word
    }
    deriving (Eq, Ord, Read, Show)

$(deriveJSON defaultOptions { fieldLabelModifier = drop 11 } ''FilesReply)

archApiUrl :: String
archApiUrl = "https://www.archlinux.org/packages"

urlParams :: [Param] -> String
urlParams = concat . map (("&"++) . showParam)

searchUrl :: Search -> Url
searchUrl s = case s of
    Name n ps        -> url ++ "?q="    ++ n ++ urlParams ps
    Exact n ps       -> url ++ "?name=" ++ n ++ urlParams ps
    Description n ps -> url ++ "?desc=" ++ n ++ urlParams ps
    where url = archApiUrl ++ "/search/json/"

apiCall :: MonadIO m => Url -> m ByteString
apiCall url = liftIO $ H.parseUrl url >>= \req -> do
    H.withManager T.tlsManagerSettings $ \mgr -> do
        B.concat <$> H.withResponse req mgr (H.brConsume . H.responseBody)

search :: (Functor m, MonadIO m) => Search -> m (Maybe SearchResult)
search s = decodeStrict <$> apiCall (searchUrl s)
