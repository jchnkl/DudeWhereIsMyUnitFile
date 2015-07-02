{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Arch where

import Data.Word
import Data.Maybe
import Control.Monad
import Control.Applicative
import qualified Data.ByteString as B

import Network.HTTP.Client (Request)
import qualified Network.HTTP.Client as H
import qualified Network.HTTP.Client.TLS as T
-- import Text.XML.Light.Types
-- import qualified Text.XML.Light as X

import Data.Aeson
import Data.Aeson.TH

import Data.Time.Clock (UTCTime)

import Common

data Pkg = Pkg
    { pkgdesc :: String
    , depends :: [String]
    , licenses :: [String]
    , last_update :: UTCTime
    , build_date :: UTCTime
    , compressed_size :: Word
    , installed_size :: Word
    , filename :: String
    , epoch :: Word
    , provides :: [String]
    , repo :: String
    , maintainers :: [String]
    , groups :: [String]
    , conflicts :: [String]
    , packager :: String
    , arch :: String
    , pkgver :: String
    , replaces :: [String]
    , pkgname :: String
    , url :: String
    , pkgbase :: String
    , pkgrel :: String
    , flag_date :: Maybe UTCTime
    }
    deriving (Eq, Ord, Read, Show)

$(deriveJSON defaultOptions ''Pkg)

data SearchResult = SearchResult
    { version :: Int
    , limit :: Int
    , valid :: Bool
    , results :: [Pkg]
    }
    deriving (Eq, Ord, Read, Show)

$(deriveJSON defaultOptions ''SearchResult)

-- https://www.archlinux.org/packages/search/json/?name=
archApiUrl :: String
archApiUrl = "https://www.archlinux.org/packages"

data Repository = Core
                | Extra
                | Testing
                | Multilib
                | MultilibTesting
                | Community
                | CommunityTesting
    deriving (Eq, Ord, Read)

instance Show Repository where
    show r = case r of
        Core -> "Core"
        Extra -> "Extra"
        Testing -> "Testing"
        Multilib -> "Multilib"
        MultilibTesting -> "Multilib-Testing"
        Community -> "Community"
        CommunityTesting -> "Community-Testing"

data Architecture = Any | I686 | X86_64
    deriving (Eq, Ord, Read)

instance Show Architecture where
    show a = case a of
        Any -> "any"
        I686 -> "i686"
        X86_64 -> "x86_64"

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
    Repository o -> "repo=" ++ show o
    Architecture o -> "arch=" ++ show o
    Maintainer o -> "maintainer=" ++ show o
    Packager o -> "packager=" ++ show o
    Flagged o -> "flagged=" ++ show o

data Search = Name String [Param]
            | Exact String [Param]
            | Description String [Param]
    deriving (Eq, Ord, Read, Show)

urlParams :: [Param] -> String
urlParams = concat . map (("&"++) . showParam)

searchUrl :: Search -> Url
searchUrl s = case s of
    Name n ps        -> url ++ "?q="    ++ n ++ urlParams ps
    Exact n ps       -> url ++ "?name=" ++ n ++ urlParams ps
    Description n ps -> url ++ "?desc=" ++ n ++ urlParams ps
    where url = archApiUrl ++ "/search/json/"

search s = do
    request >>= \req -> do
        H.withManager T.tlsManagerSettings $ \mgr -> do
            -- concat . maybeToList . fmap results . decodeStrict . B.concat
            -- decodeStrict . B.concat
            B.concat
                <$> H.withResponse req mgr (H.brConsume . H.responseBody)
    where
    -- toResult :: ByteString -> SearchResult
    -- toResult = 
    request = H.parseUrl $ searchUrl s


