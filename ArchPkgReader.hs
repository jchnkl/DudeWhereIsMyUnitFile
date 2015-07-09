-- ghc -O2 \
-- -package-db /home/jkeil/.cabal-sandbox/x86_64-linux-ghc-7.8.4-packages.conf.d \
-- --make ArchPkgReader.hs

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module ArchPkgReader where

import System.Environment (getArgs)
import System.Posix.Types (FileOffset)
import System.Posix.Files (getFileStatus, fileSize)
import qualified System.IO as IO


import Data.List
import Control.Monad
import Control.Applicative
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
-- import Control.Monad.Trans.Control

-- import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
-- import qualified Data.ByteString.Lazy.Char8 as B8
import Data.Conduit
import Data.Conduit.Lzma
import qualified Data.Conduit.Combinators as Cmb
import Codec.Archive.Tar (Entries, FormatError)
import Control.Monad.Trans.Resource (runResourceT)
-- import Control.Monad.Trans.Resource.Internal (MonadResource)

-- import Codec.Archive.Tar (Entries(..))
import qualified Codec.Archive.Tar as Tar
import Codec.Archive.Tar.Entry (Entry(..))
import qualified Codec.Archive.Tar.Entry as Tar

import Common.Functions
-- import Debug.Trace

readLzma :: (MonadBaseControl IO m, MonadIO m, MonadThrow m)
         => FilePath -> m BL.ByteString
readLzma fp = runResourceT $ Cmb.sourceFile fp $= decompress Nothing $$ Cmb.sinkLazy

-- readPkgEntries :: (MonadBaseControl IO m, MonadThrow m, MonadIO m)
--                => FilePath -> m (Entries FormatError)
-- readPkgEntries = fmap Tar.read . readLzma

isUnitEntry :: Entry -> Bool
isUnitEntry fp = isPrefixOf "usr/lib/systemd" p && isSuffixOf ".service" p
    where p = Tar.entryPath fp

-- hasUnitFiles :: FilePath -> IO Bool
-- hasUnitFiles = fmap (foldr (||) False . map isUnitEntry . toEntryList) . readPkgEntries

isTar :: FilePath -> Bool
isTar = isSuffixOf ".tar"

isGz :: FilePath -> Bool
isGz = isSuffixOf ".gz"

isLzma :: FilePath -> Bool
isLzma = isSuffixOf ".xz"

-- filterLzma :: [FilePath] -> [FilePath]
-- filterLzma = filter isLzma

maxSize :: Int
maxSize = 1

isSmallerThan :: FileOffset -> FilePath -> IO Bool
isSmallerThan n = fmap ((n >) . fileSize) . getFileStatus

-- isUnitEntry :: Entry -> Bool
-- isUnitEntry = Tar.entryPath

-- unitFileCheck :: FilePath -> IO Bool
-- unitFileCheck fp = isSmallerThan (fromIntegral maxSize * 1024 * 1024) fp >>= \b -> do
--     if b then hasUnitFiles fp
--          else return b

unpack :: FilePath -> IO (Entries FormatError)
unpack fp = decomp >>= return . Tar.read
    where
    decomp | isTar  fp = BL.readFile fp
           | isGz   fp = return "" -- fail "FIXME: implement GZ support"
           | isLzma fp = readLzma fp
           | otherwise = return ""

findPackages :: (Entries FormatError -> IO Bool) -> [FilePath] -> IO [FilePath]
findPackages f = findpkg []
    where
    findpkg r [] = return r
    findpkg r (fp:fps) =
        unpack fp >>= f >>= \b -> if b then findpkg (fp:r) fps else findpkg r fps

matchUnitFiles :: Entries FormatError -> IO Bool
matchUnitFiles = return
               . foldr (||) False
               . Tar.foldEntries ((:) . isUnitEntry) [] (const [])

findPkgsWithUnitFiles :: FilePath -> IO [FilePath]
findPkgsWithUnitFiles base = do
    map (base </>) <$> Tar.getDirectoryContentsRecursive base
        >>= filterM (isSmallerThan (fromIntegral maxSize * 1024 * 1024))
        >>= findPackages matchUnitFiles

main :: IO ()
main = do
    args <- getArgs

    let base = (args !! 0)

    map (base </>) <$> Tar.getDirectoryContentsRecursive base
        >>= filterM (isSmallerThan (fromIntegral maxSize * 1024 * 1024))
        >>= findPackages matchUnitFiles
        >>= mapM_ (IO.hPutStrLn IO.stderr)
