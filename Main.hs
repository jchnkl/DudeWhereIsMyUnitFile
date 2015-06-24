import Control.Monad
import Control.Applicative
import Data.Hashable (Hashable(..))
import qualified Data.List as L
import qualified Data.HashSet as S
import System.Environment (getArgs)
import System.FilePath.Posix ((</>))
import qualified System.Directory as D

import Rpm

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

susePkgs :: IO [Pkg]
susePkgs = pkgList s_pkg_path

-- getPkgName :: PkgPath -> IO Pkg
-- getPkgName fp = 

-- foo :: Pkg -> String
-- foo (Pkg n _) = n ++ ".service"

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

getPackages :: IO (S.HashSet Pkg)
getPackages = do
    c_pkgs <- S.fromList <$> communityPkgs
    p_pkgs <- S.fromList <$> packagesPkgs
    s_pkgs <- S.fromList <$> susePkgs
    return $ c_pkgs `S.union` p_pkgs `S.union` s_pkgs

filterWithServiceFile :: [Pkg] -> IO [Pkg]
filterWithServiceFile = filterM hasServiceFile

-- main :: IO ()
-- main = do
--     unlines . L.nub <$> serviceFiles >>= writeFile "/tmp/serviceFiles.txt"
--     where serviceFiles = communityPkgs >>= fmap concat . mapM locateServiceFiles

main :: IO ()
main = do
    args <- getArgs
    print args
    rpmFileList (head args) >>= putStrLn . unlines
