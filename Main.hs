{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Maybe (isJust, fromMaybe)
import Control.Monad
import Control.Applicative
import Control.Concurrent
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe
import Data.Hashable (Hashable(..))
import qualified Data.List as L
import qualified Data.HashSet as S
import qualified System.IO as IO
import System.Environment (getArgs)
import System.FilePath.Posix ((</>))
import qualified System.Directory as D

import Formatting
import Formatting.Formatters
import System.Clock (Clock(..), TimeSpec(..))
import qualified System.Clock as C

import Common.HTTP
import Common.Types hiding ((</>))
import Common.Functions
import qualified Rpm as R
-- import OBS (UserName, Password, PackageName)
import qualified OBS as O
import Arch (Search(..), Param(..))
import qualified Arch as A

type PkgName = String
type PkgPath = FilePath

data Pkg = Pkg
    { name :: PkgName
    , path :: PkgPath
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

    -- p_pkgs <- S.fromList <$> packagesPkgs
    -- s_pkgs <- S.fromList <$> susePkgs
    -- return $ c_pkgs `S.union` p_pkgs `S.union` s_pkgs

-- getPackages :: IO (S.HashSet Pkg)
-- getPackages = do
--     c_pkgs <- S.fromList <$> communityPkgs
--     p_pkgs <- S.fromList <$> packagesPkgs
--     s_pkgs <- S.fromList <$> susePkgs
--     return $ c_pkgs `S.union` p_pkgs `S.union` s_pkgs

filterWithServiceFile :: [Pkg] -> IO [Pkg]
filterWithServiceFile = filterM hasServiceFile

measureDuration :: MonadIO m => Clock -> m a -> m (a, TimeSpec)
measureDuration c m = do
    start <- liftIO $ C.getTime c
    a <- m
    end <- liftIO $ C.getTime c
    -- return (a, TimeSpec (C.sec end - C.sec start) (C.nsec end - C.nsec start))
    return (a, end - start)

floatTime :: TimeSpec -> Float
floatTime (TimeSpec s n) = fromIntegral s + fromIntegral n / 10^9

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust ma f = case ma of
    Nothing -> return ()
    Just a  -> f a

putStrStatus :: MonadIO m => String -> m ()
putStrStatus s = liftIO $ do
    -- http://stackoverflow.com/a/8953814
    -- https://en.wikipedia.org/wiki/ANSI_escape_code
    -- ESC[ == CSI
    IO.hPutStr IO.stdout $ "\r\ESC[K" ++ s
    IO.hFlush IO.stdout

main :: IO ()
main = do
    args <- getArgs
    guard $ length args == 2
    let username = args !! 0
        password = args !! 1
    --     -- package  = args !! 2

    -- susePkgs <- getPackageList (SourcePath s_pkg_path)

    -- O.getRpmRoute (O.basicAuth username password) package >>= \case
    --     Nothing    -> print $ "Couldn't find rpm for " ++ package
    --     Just route -> let url = O.obsApiAuthUrl username password ++ "/" ++ route
    --                   in R.rpmFileList url >>= putStrLn . unlines

    susePkgs <- getPackageList (SourcePath s_pkg_path)
    let npkgs = fromIntegral $ length susePkgs
    putStrLn $ "got " ++ show npkgs ++ " packages.."

    -- pkgs <- getPackageList (SourcePath s_pkg_path) >>= mapM archSearch
    -- print $ length pkgs

    -- putStr "\n"

    startTime <- C.getTime Monotonic

    flip runStateT 0 $ do
        forM_ (zip [1..] susePkgs) $ \(n, pkg) -> do

            -- (mr, d) <- fmap floatTime <$> measureDuration Monotonic (archSearch pkg)
            (mr, d) <- fmap floatTime <$> measureDuration Monotonic (hasUnitFiles pkg)
            modify (+d)
            avg <- gets (/n)

            -- when (isJust mr) $ do
            --     files <- rpmPkgFileList username password pkg
            --     filter (isSuffixOf ".service") files

            now <- liftIO $ C.getTime Monotonic

            let tl = ppFloatTime $ (npkgs - n) * avg
                el = ppFloatTime $ floatTime (now - startTime)
                ps = "time elapsed: " ++ el ++ ", time left: " ++ tl

            putStrStatus $ wrapParens ps ++ " " ++ status pkg (mr, d)

                -- ps = "avg: " ++ show avg ++ "s"
                --   ++ ", pkgs left: " ++ show (npkgs - n)
                --   -- ++ ", time left: ~" ++ show tl ++ "s"
                --   ++ ", time left: " ++ ppTimeSpec tl

            -- O.getRpmRoute (O.basicAuth username password) package >>= \case
            --     Nothing    -> print $ "Couldn't find rpm for " ++ package
            --     Just route -> let url = O.obsApiAuthUrl username password ++ "/" ++ route
            --                   in R.rpmFileList url >>= putStrLn . unlines


    putStr "\n"

    -- O.getRpmRoute (O.basicAuth username password) package >>= \case
    --     Nothing    -> print $ "Couldn't find rpm for " ++ package
    --     Just route -> let url = O.obsApiAuthUrl username password ++ "/" ++ route
    --                   in R.rpmFileList url >>= putStrLn . unlines

    where
    wrapParens s = "(" ++ s ++ ")"

    -- status pkg (mr, d) = case (map A.pkg_pkgname . A.searchReply_results <$> mr) of
    --     Nothing   -> "no pkgs found for " ++ pkg
    --     Just []   -> "no pkgs found for " ++ pkg
    --     Just pkgs -> "found " ++ show pkgs ++ " in " ++ show d ++ "s"

    status pkg (mr, d) | mr        = pkg ++ " has unit files\n"
                       | otherwise = pkg ++ " has NO unit files"

    -- archSearch n = A.search $ A.Exact n [A.Architecture A.X86_64]
    archSearch n = A.search $ A.Exact n [A.Architecture A.X86_64]

    packageNames n = fmap (map A.pkg_pkgname . A.searchReply_results)
                     <$> A.search (A.Exact n [A.Architecture A.X86_64])

    packageFiles n = fmap A.filesReply_files
                     <$> A.files A.Core A.X86_64 n

    unitFiles n = fmap (filter isUnitFile . concat . fromMaybe []) . runMaybeT $ do
        packageNames n >>= hoistMaybeT >>= mapM ((hoistMaybeT =<<) . packageFiles)

    hasUnitFiles = fmap (not . null) . unitFiles


    -- hasUnitFiles n = fmap isJust . runMaybeT $ do
    --     fmap (A.searchReply_results) search n
    --         >>= hoistMaybeT
    --         >>= mapM ((hoistMaybeT =<<) . A.files A.Core A.Architecture)

    -- search :: (Functor m, MonadIO m) => Search -> m (Maybe SearchReply)
    -- files :: (Functor m, MonadIO m) => Repository -> Architecture -> PackageName -> m (Maybe FilesReply)

isUnitFile :: FilePath -> Bool
isUnitFile = L.isSuffixOf ".service"

rpmPkgFileList :: UserName -> Password -> PackageName -> IO [String]
rpmPkgFileList user pass pkg = O.getRpmRoute auth pkg >>= fileList . fmap makeUrl
    where
    fileList murl = case murl of
        Nothing  -> return []
        Just url -> R.rpmFileList url
    auth = basicAuth user pass
    makeUrl route = O.obsApiAuthUrl user pass ++ "/" ++ route

ppFloatTime :: Float -> String
ppFloatTime ft = formatToString
                 (left 2 '0' % ":" % left 2 '0' % ":" % left 2 '0')
                 (show hours) (show mins) (show secs)
    where
    (hours, minfrac) = properFraction (ft / 3600)
    (mins, secfrac)  = properFraction (minfrac * 60)
    secs             = round $ secfrac * 60
