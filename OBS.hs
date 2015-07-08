{-# LANGUAGE OverloadedStrings #-}

module OBS where

import Data.Maybe
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Catch (Exception, MonadCatch)
import Control.Monad.Except (MonadError)
import qualified Data.ByteString.Char8 as B

import qualified Network.HTTP.Client as H
import qualified Network.HTTP.Client.TLS as T
import Text.XML.Light.Types
import qualified Text.XML.Light as X

import Common.Http (Auth)
import qualified Common.Http as Http
import Common.Types
import Common.Functions (hoistMaybeT)
import qualified Rpm as Rpm

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
filterByAttr p (e@(Element _ attrs _ _):elems)
    | any p attrs = e : filterByAttr p elems
    | otherwise   = filterByAttr p elems

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
getRpmRoute auth pkg = (mkUrl =<<) . listToMaybe . filter p <$> findRpms auth pkg
    where
    arch = "x86_64"
    repository = "openSUSE_Factory"
    p e = isArch arch e && isRepository repository e
    mkUrl e = do
        prj <- getProject e
        fn  <- getFileName e
        return $ "/build/" ++ prj ++ "/" ++ repository ++ "/" ++ arch ++ "/" ++ pkg ++ "/" ++ fn

getPkgFiles :: UserName -> Password -> PackageName -> IO (Maybe [FilePath])
getPkgFiles user pass pkg = runMaybeT $ do
    liftIO . Rpm.rpmFileList . url
        =<< hoistMaybeT
        =<< liftIO (getRpmRoute (Http.basicAuth user pass) pkg)
    where url route = obsApiAuthUrl user pass </> route

factoryPackages :: (Exception e, MonadCatch f, MonadError e f, MonadIO f, Functor f)
                => UserName -> Password -> f [String]
factoryPackages user pass = xml <$> Http.apiCall (Just auth) url
    where
    auth = Http.basicAuth user pass
    url = obsApiUrl </> "source" </> "openSUSE:Factory"
    xml = map X.attrVal -- Attr -> String
        -- [Element] -> [[Attr]] -> [Attr]
        . concatMap X.elAttribs
        -- [Content] -> [Element]
        . X.onlyElems
        -- [Element] -> [[Content]] -> [Content]
        . concatMap X.elContent
        -- [Content] -> [Element]
        . X.onlyElems
        -- ByteString -> [Content]
        . X.parseXML
