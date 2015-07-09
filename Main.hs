{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Maybe (isJust, isNothing, fromMaybe, maybeToList)
import Control.Monad
import Control.Applicative
import Control.Concurrent
import Control.Monad.Reader
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe
import qualified Data.ByteString.Char8 as B
import Data.Hashable (Hashable(..))
import qualified Data.List as L
import qualified Data.HashSet as S
import qualified System.IO as IO
import System.Environment (getArgs, getProgName)
import qualified System.Directory as D

import Formatting
import Formatting.Formatters
import System.Clock (Clock(..), TimeSpec(..))
import qualified System.Clock as C

import Common.Http
import Common.Types
import Common.Functions
import qualified Rpm as R
-- import OBS (UserName, Password, PackageName)
import qualified OBS as O
import Arch (Search(..), Param(..), Architecture(..), Repository(..))
import qualified Arch as A
import qualified ArchPkgReader as APR

type PkgName = String
type PkgPath = FilePath

type AppMonad m = (Functor m, MonadReader Cred m, MonadIO m)

data Pkg = Pkg
    { name :: PkgName
    , path :: PkgPath
    }
    deriving (Eq, Ord, Read, Show)

data Args = Args
    { user    :: String
    , passwd  :: String
    , outFile :: Maybe String
    }
    deriving (Eq, Ord, Read, Show)

instance Hashable Pkg where
    hashWithSalt s (Pkg n _) = hashWithSalt s n

comparePkgName :: Pkg -> Pkg -> Bool
comparePkgName p1 p2 = name p1 == name p2

c_pkg_path :: PkgPath
c_pkg_path = "/home/jkeil/tmp/community"

p_pkg_path :: PkgPath
p_pkg_path = "/home/jkeil/tmp/packages"

s_pkg_path :: PkgPath
s_pkg_path = "/mounts/langsam/SRC-unpacked/all"

toPkg :: PkgName -> PkgPath -> Pkg
toPkg = Pkg

pkgList :: FilePath -> IO [Pkg]
pkgList fp = map (flip toPkg fp) <$> D.getDirectoryContents fp

communityPkgs :: IO [Pkg]
communityPkgs = pkgList c_pkg_path

packagesPkgs :: IO [Pkg]
packagesPkgs = pkgList p_pkg_path

-- susePkgs :: IO [Pkg]
-- susePkgs = pkgList s_pkg_path

locateServiceFiles :: Pkg -> IO [FilePath]
locateServiceFiles (Pkg n p) = do
    cs <- D.getDirectoryContents (p </> n)
    if "trunk" `elem` cs
        then serviceFiles <$> D.getDirectoryContents (p </> n </> "trunk")
        else return . serviceFiles $ cs
    where
    serviceFiles = filter (L.isSuffixOf ".service")

hasServiceFile :: Pkg -> IO Bool
hasServiceFile pkg = (>0) . length <$> locateServiceFiles pkg

data Source = SourcePath FilePath
    deriving (Eq, Ord, Read, Show)

getPackageList :: Source -> IO [String]
getPackageList = \case
    SourcePath fp -> D.getDirectoryContents fp

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust ma f = case ma of
    Nothing -> return ()
    Just a  -> f a

measureDuration :: MonadIO m => Clock -> m a -> m (a, TimeSpec)
measureDuration c m = do
    start <- liftIO $ C.getTime c
    a <- m
    end <- liftIO $ C.getTime c
    -- return (a, TimeSpec (C.sec end - C.sec start) (C.nsec end - C.nsec start))
    return (a, end - start)

toFloatTime :: TimeSpec -> Float
toFloatTime (TimeSpec s n) = fromIntegral s + fromIntegral n / 10^9

ppFloatTime :: Float -> String
ppFloatTime ft = formatToString
                 (left 2 '0' % ":" % left 2 '0' % ":" % left 2 '0')
                 (show hours) (show mins) (show secs)
    where
    (hours, minfrac) = properFraction (ft / 3600)
    (mins, secfrac)  = properFraction (minfrac * 60)
    secs             = round $ secfrac * 60

putStrStatus :: MonadIO m => String -> m ()
putStrStatus s = liftIO $ do
    -- http://stackoverflow.com/a/8953814
    -- https://en.wikipedia.org/wiki/ANSI_escape_code
    -- ESC[ == CSI
    IO.hPutStr IO.stdout $ "\r\ESC[K" ++ s
    IO.hFlush IO.stdout

getArchFiles :: AppMonad m => PackageName -> m [FilePath]
getArchFiles pkg = toFileList <$> A.files Core X86_64 pkg
    where toFileList = concat . maybeToList . fmap A.filesReply_files

getSuseFiles :: AppMonad m => PackageName -> m [FilePath]
getSuseFiles pkg = concat . maybeToList <$> O.getPkgFiles pkg

parseArgs :: [String] -> Maybe Args
parseArgs args | length args == 2 = Just $ Args (args !! 0) (args !! 1) Nothing
               | length args == 3 = Just $ Args (args !! 0) (args !! 1) (Just $ args !! 2)
               | otherwise        = Nothing

usage :: Maybe () -> IO ()
usage m = when (isNothing m) $ do
    getProgName >>= putStrLn . (++ " <username> <password> [<output file>]")

main :: IO ()
main = usage <=< runMaybeT $ do
    args <- hoistMaybeT =<< parseArgs <$> liftIO getArgs
    let cred = Cred (user args) (passwd args)

    susePkgs <- O.factoryPackages (user args) (passwd args)

    liftIO . putStrLn $ "got " ++ show (length susePkgs) ++ " packages.."

    startTime <- liftIO $ C.getTime Monotonic

    flip runReaderT cred $ flip runStateT 0 $ do
        forM_ (zip [1::Int ..] susePkgs) $ \(n, pkg) -> do

            (needUnits, d) <- fmap (toFloatTime <$>) . measureDuration Monotonic $ do
                archHasUnitFiles <- hasUnitFiles <$> getArchFiles pkg
                if archHasUnitFiles
                    then not . hasUnitFiles <$> getSuseFiles pkg
                    else return False

            modify (+d)
            avg <- gets (/ fromIntegral n)

            now <- liftIO $ C.getTime Monotonic

            let tl = ppFloatTime $ fromIntegral (length susePkgs - n) * avg
                el = ppFloatTime $ toFloatTime (now - startTime)
                ps = "avg per pkg: " ++ show avg ++ "s"
                  ++ ", elapsed: " ++ el
                  ++ ", left: " ++ tl

            putStrStatus $ show n ++ "/" ++ show (length susePkgs) ++ ", " ++ ps

            when needUnits . liftIO $ printPkgInfo pkg (outFile args)

    liftIO $ putStr "\n"

    where
    wrapParens s = "(" ++ s ++ ")"
    showNeedUnits b = (if b then "needs" else "doesn't need") ++ " unit files"

    printPkgInfo pkg = \case
        Nothing -> IO.hPutStr IO.stdout (pkg ++ "\n") >> IO.hFlush IO.stdout
        Just fp -> B.appendFile fp (B.pack $ pkg ++ "\n")
