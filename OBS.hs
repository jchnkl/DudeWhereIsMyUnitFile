{-# LANGUAGE OverloadedStrings #-}

module OBS where

import Data.Maybe
import Control.Monad
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import qualified Data.ByteString.Char8 as B

import qualified Network.HTTP.Client as H
import qualified Network.HTTP.Client.TLS as T
import Text.XML.Light.Types
import qualified Text.XML.Light as X

import Debug.Trace

import Common.HTTP
import Common.Types
import Common.Functions

obsApiUrl :: Url
obsApiUrl = "https://api.opensuse.org"

obsApiAuthUrl :: UserName -> Password -> Url
obsApiAuthUrl u p = "https://" ++ u ++ ":" ++ p ++ "@" ++ "api.opensuse.org"

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
