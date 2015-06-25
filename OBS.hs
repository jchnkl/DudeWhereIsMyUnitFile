{-# LANGUAGE OverloadedStrings #-}

module OBS where

import Data.Maybe
import Control.Monad
import Control.Applicative
import qualified Data.ByteString.Char8 as B

import Network.HTTP.Client (Request)
import qualified Network.HTTP.Client as H
import qualified Network.HTTP.Client.TLS as T
import Text.XML.Light.Types
import qualified Text.XML.Light as X

import Debug.Trace

type Auth = Request -> Request
type Url = String
type Route = String
type UserName = String
type Password = String
type ProjectName = String
type PackageName = String

obsApiUrl :: Url
obsApiUrl = "https://api.opensuse.org"

obsApiAuthUrl :: UserName -> Password -> Url
obsApiAuthUrl u p = "https://" ++ u ++ ":" ++ p ++ "@" ++ "api.opensuse.org"

basicAuth :: UserName -> Password -> Auth
basicAuth u p = H.applyBasicAuth (B.pack u) (B.pack p)

findDevelProject :: Auth -> PackageName -> IO [(ProjectName, PackageName)]
findDevelProject auth pkg = do
    sendRequest >>= \request -> do
        H.withManager T.tlsManagerSettings $ \manager -> do
            H.withResponse request manager processResponse

    where
    searchUrl = "/search/package?match=@name='" ++ pkg ++ "'"

    sendRequest = auth <$> H.parseUrl (obsApiUrl ++ searchUrl)

    processResponse r = concatMap find . X.parseXML . B.concat
                        <$> H.brConsume (H.responseBody r)

    isDevel (Element qn as _ _) = qName qn == "devel" && length as == 2

    find (Text _) = []
    find (CRef _) = []
    find (Elem e@(Element _ attrs content _))
        | isDevel e = [(attrVal (attrs !! 0), attrVal (attrs !! 1))]
        | otherwise = concatMap find content


-- <binary name="rpcbind" project="network" package="rpcbind"
-- repository="openSUSE_Tumbleweed*" version="0.2.3" release="74.4" arch="i586"
-- filename="rpcbind-0.2.3-74.4.i586.rpm"
-- filepath*="network/openSUSE_Tumbleweed/i586/rpcbind-0.2.3-74.4.i586.rpm"
-- baseproject="openSUSE:Factor*y" type="rpm" />

mkAttrKey :: String -> QName
mkAttrKey k = QName k Nothing Nothing

filterByAttr :: (Attr -> Bool) -> [Element] -> [Element]
filterByAttr _ [] = []
filterByAttr pred (e@(Element _ attrs _ _):elems)
    | any pred attrs = e : filterByAttr pred elems
    | otherwise      = filterByAttr pred elems

findRpms :: Auth -> PackageName -> IO [Element]
findRpms auth pkg = H.withManager T.tlsManagerSettings $ \manager -> do
    auth <$> H.parseUrl url >>= \request -> do
        H.withResponse request manager process
    where
    url = obsApiUrl ++ "/search/published/binary/id?match=@name='" ++ pkg ++ "'"
    collectionElements = concatMap (X.onlyElems . elContent) . X.onlyElems
    process = fmap (collectionElements . X.parseXML . B.concat)
            . H.brConsume . H.responseBody

isArch :: String -> Element -> Bool
isArch arch = any ((arch==) . attrVal) . elAttribs

isRepository :: String -> Element -> Bool
isRepository repository = any ((repository==) . attrVal) . elAttribs

getFileName :: Element -> Maybe FilePath
getFileName = X.findAttr (mkAttrKey "filename")

getProject :: Element -> Maybe ProjectName
getProject = X.findAttr (mkAttrKey "project")

getRpmRoute :: Auth -> PackageName -> IO (Maybe Route)
getRpmRoute auth pkg = (mkUrl =<<) . listToMaybe . filter pred <$> findRpms auth pkg
    where
    arch = "x86_64"
    repository = "openSUSE_Factory"
    pred e = isArch arch e && isRepository repository e
    mkUrl e = do
        prj <- getProject e
        fn  <- getFileName e
        return $ "/build/" ++ prj ++ "/" ++ repository ++ "/" ++ arch ++ "/" ++ pkg ++ "/" ++ fn

-- https://build.opensuse.org/build/network/openSUSE_Tumbleweed/x86_64/rpcbind/rpcbind-0.2.3-74.4.x86_64.rpm



-- showAttr (Attr k v) = qName k ++ "=\"" ++ v ++ "\""
-- showStuff (Elem (Element n as c _)) =
--     "<" ++ qName n ++ " " ++ (unwords $ map showAttr as) ++ " />"
-- -- showStuff (Elem (Element n as c _)) = qName n ++ "\n" ++ (unlines $ map showAttr as)
-- showStuff _ = ""

-- rpmUrl :: (ProjectName, PackageName) -> Url
-- rpmUrl (prj, pkg) = obsApiUrl
--                  ++ "/build/" ++ prj ++ "/openSUSE_Tumbleweed/x86_64/"
--                  ++ pkg ++ "/rpcbind-0.2.3-74.4.x86_64.rpm"


-- https://www.archlinux.org/packages/core/x86_64/rpcbind/files/json/
-- https://build.opensuse.org/build/network/openSUSE_Tumbleweed/x86_64/rpcbind/rpcbind-0.2.3-74.4.x86_64.rpm

-- search packages:
-- read user; read password; echo $user:$password; \
-- curl --user $user:$password \
--      -XGET "https://api.opensuse.org/search/package?match=@name='rpcbind'"
