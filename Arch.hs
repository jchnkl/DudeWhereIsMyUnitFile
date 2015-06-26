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
-- import System.Locale (defaultTimeLocale)

-- data MyTime = MyTime UTCTime
    -- deriving (Eq, Ord, Read, Show)

-- instance Read MyTime

data Pkg = Pkg
    { pkgdesc :: String
    , depends :: [String]
    , licenses :: [String]
    -- , last_update :: "2015-05-02T09:48:14.746Z",
    , last_update :: UTCTime
    -- , build_date :: "2015-04-27T18:16:35Z",
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
    -- "flagdate": "2013-07-03T06:46:29.620Z"
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

type Url = String

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

-- search :: Search -> IO [Pkg]
-- search :: Search -> IO (Maybe SearchResult)
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


-- class SearchRequestClass a where
--     searchRequest

-- mkSearchRequest :: String -> SearchRequest
-- mkSearchRequest query
-- /search/json/?name=

-- searchPkg :: String -> IO PkgSearchReply
-- searchPkg pkg = do
--     sendRequest >>= \request -> do
--         H.withManager T.tlsManagerSettings $ \manager -> do
--             H.withResponse request manager processResponse
--
--     where
--     searchUrl = "/search/package?match=@name='" ++ pkg ++ "'"
--
--     sendRequest = auth <$> H.parseUrl (obsApiUrl ++ searchUrl)
--
--     processResponse r = concatMap find . X.parseXML . B.concat
--                         <$> H.brConsume (H.responseBody r)
--
--     isDevel (Element qn as _ _) = qName qn == "devel" && length as == 2
--
--     find (Text _) = []
--     find (CRef _) = []
--     find (Elem e@(Element _ attrs content _))
--         | isDevel e = [(attrVal (attrs !! 0), attrVal (attrs !! 1))]
--         | otherwise = concatMap find content

-- import Debug.Trace

-- type Auth = Request -> Request
-- type Url = String
-- type Route = String
-- type UserName = String
-- type Password = String
-- type ProjectName = String
-- type PackageName = String



-- obsApiUrl :: Url
-- obsApiUrl = "https://api.opensuse.org"
--
-- obsApiAuthUrl :: UserName -> Password -> Url
-- obsApiAuthUrl u p = "https://" ++ u ++ ":" ++ p ++ "@" ++ "api.opensuse.org"
--
-- basicAuth :: UserName -> Password -> Auth
-- basicAuth u p = H.applyBasicAuth (B.pack u) (B.pack p)
--
-- findDevelProject :: Auth -> PackageName -> IO [(ProjectName, PackageName)]
-- findDevelProject auth pkg = do
--     sendRequest >>= \request -> do
--         H.withManager T.tlsManagerSettings $ \manager -> do
--             H.withResponse request manager processResponse
--
--     where
--     searchUrl = "/search/package?match=@name='" ++ pkg ++ "'"
--
--     sendRequest = auth <$> H.parseUrl (obsApiUrl ++ searchUrl)
--
--     processResponse r = concatMap find . X.parseXML . B.concat
--                         <$> H.brConsume (H.responseBody r)
--
--     isDevel (Element qn as _ _) = qName qn == "devel" && length as == 2
--
--     find (Text _) = []
--     find (CRef _) = []
--     find (Elem e@(Element _ attrs content _))
--         | isDevel e = [(attrVal (attrs !! 0), attrVal (attrs !! 1))]
--         | otherwise = concatMap find content
--
-- mkAttrKey :: String -> QName
-- mkAttrKey k = QName k Nothing Nothing
--
-- filterByAttr :: (Attr -> Bool) -> [Element] -> [Element]
-- filterByAttr _ [] = []
-- filterByAttr pred (e@(Element _ attrs _ _):elems)
--     | any pred attrs = e : filterByAttr pred elems
--     | otherwise      = filterByAttr pred elems
--
-- findRpms :: Auth -> PackageName -> IO [Element]
-- findRpms auth pkg = H.withManager T.tlsManagerSettings $ \manager -> do
--     auth <$> H.parseUrl url >>= \request -> do
--         H.withResponse request manager process
--     where
--     url = obsApiUrl ++ "/search/published/binary/id?match=@name='" ++ pkg ++ "'"
--     collectionElements = concatMap (X.onlyElems . elContent) . X.onlyElems
--     process = fmap (collectionElements . X.parseXML . B.concat)
--             . H.brConsume . H.responseBody
--
-- isArch :: String -> Element -> Bool
-- isArch arch = any ((arch==) . attrVal) . elAttribs
--
-- isRepository :: String -> Element -> Bool
-- isRepository repository = any ((repository==) . attrVal) . elAttribs
--
-- getFileName :: Element -> Maybe FilePath
-- getFileName = X.findAttr (mkAttrKey "filename")
--
-- getProject :: Element -> Maybe ProjectName
-- getProject = X.findAttr (mkAttrKey "project")
--
-- getRpmRoute :: Auth -> PackageName -> IO (Maybe Route)
-- getRpmRoute auth pkg = (mkUrl =<<) . listToMaybe . filter pred <$> findRpms auth pkg
--     where
--     arch = "x86_64"
--     repository = "openSUSE_Factory"
--     pred e = isArch arch e && isRepository repository e
--     mkUrl e = do
--         prj <- getProject e
--         fn  <- getFileName e
--         return $ "/build/" ++ prj ++ "/" ++ repository ++ "/" ++ arch ++ "/" ++ pkg ++ "/" ++ fn
