{-# LANGUAGE FlexibleContexts #-}

-- module ArchPkgReader where

import System.Environment (getArgs)
import System.FilePath.Posix ((</>))
import System.Posix.Types (FileOffset)
import System.Posix.Files (getFileStatus, fileSize)
import qualified System.IO as IO


import Data.List
import Control.Monad
import Control.Applicative
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Control.Monad.Trans.Control

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as B8
import Data.Conduit
import Data.Conduit.Lzma
import qualified Data.Conduit.Combinators as Cmb
import Codec.Archive.Tar (Entries, FormatError)
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Trans.Resource.Internal (MonadResource)

import Codec.Archive.Tar (Entries(..))
import qualified Codec.Archive.Tar as Tar
import Codec.Archive.Tar.Entry (Entry(..), EntryContent(..))
import qualified Codec.Archive.Tar.Entry as Tar

import Debug.Trace

-- readPkg :: FilePath -> ByteString
-- readPkg = sourceFile

-- tarConduitRead :: Monad m => Conduit BS.ByteString m (Entries FormatError)
-- tarConduitRead = Cmb.mapM (return . strictTarRead)
--     where strictTarRead = Tar.read . BL.fromChunks . (:[])

-- pkgEntries :: (MonadBaseControl IO m, MonadThrow m, MonadIO m)
--                => FilePath -> m (Maybe (Entries FormatError))

-- pkgEntries :: MonadResource m => FilePath -> ConduitM a (Entries FormatError) m ()
-- pkgEntries fp = Cmb.sourceFile fp $= decompress Nothing $= tarConduitRead
readLzma :: (MonadBaseControl IO m, MonadIO m, MonadThrow m)
         => FilePath -> m BL.ByteString
         -- => FilePath -> m BS.ByteString
readLzma fp = runResourceT $ Cmb.sourceFile fp $= decompress Nothing $$ Cmb.sinkLazy
-- readLzma fp = runResourceT $ Cmb.sourceFile fp $= decompress Nothing $$ fmap BS.concat Cmb.sinkList

readPkgEntries :: (MonadBaseControl IO m, MonadThrow m, MonadIO m)
               => FilePath -> m (Entries FormatError)
-- readPkgEntries = fmap (Tar.read . BL.fromChunks . (:[])) . readLzma
readPkgEntries = fmap Tar.read . readLzma
-- readPkgEntries = undefined

    -- Cmb.sourceFile fp $= decompress Nothing $= tarConduitRead $= await

-- readPkgEntries :: (MonadBaseControl IO m, MonadThrow m, MonadIO m)
--                => FilePath -> m (Maybe (Entries FormatError))
-- readPkgEntries fp = runResourceT $ runConduit $ do
--     Cmb.sourceFile fp $= decompress Nothing $= tarConduitRead $= await

-- foo fp = runResourceT $ runConduit $ do
--     Cmb.sourceFile fp $= decompress Nothing $= Cmb.sinkFile "/tmp/foo"

-- getEntries :: Entries e -> [Tar.TarPath]
-- getEntries = Tar.foldEntries (\e ls -> e : ls) [] (const [])
-- getEntries :: Show e => Entries e -> [Either String Entry]
-- getEntries = Tar.foldEntries (\e ls -> Right e : ls) [] (\e -> [Left $ show e])

toEntryList :: Entries e -> [Entry]
toEntryList = Tar.foldEntries (:) [] (const [])

-- myFoldEntries :: (Entry -> a -> a) -> a -> (e -> a) -> Entries e -> a
-- myFoldEntries f acc err entries = case entries of
--     Done -> acc
--     Fail e -> err e
--     -- Fail e -> myFoldEntries f err err entries
--     Next entry entries' -> myFoldEntries f (f entry acc) err entries'

-- showEntry :: Entry -> Maybe String
-- showEntry entry = case Tar.entryContent entry of
--     -- NormalFile n _ -> Just $ B8.unpack n
--     NormalFile n _ -> Just . Tar.fromTarPath . Tar.entryTarPath $ entry
--     Directory      -> Just . Tar.fromTarPath . Tar.entryTarPath $ entry
--     SymbolicLink l -> Just $ Tar.fromLinkTarget l
--     HardLink l     -> Just $ Tar.fromLinkTarget l
--     _              -> Nothing

-- showEither :: (e -> String) -> (a -> String) -> Either e a -> String
-- showEither ef _ (Left e)  = ef e
-- showEither _ af (Right a) = af a

isUnitFile :: Entry -> Bool
isUnitFile fp = isPrefixOf "usr/lib/systemd" p && isSuffixOf ".service" p
    where p = Tar.entryPath fp

hasUnitFiles :: FilePath -> IO Bool
hasUnitFiles = fmap (foldr (||) False . map isUnitFile . toEntryList) . readPkgEntries

filterLzma :: [FilePath] -> [FilePath]
filterLzma = filter (isSuffixOf ".xz")

maxSize :: Int
maxSize = 1

isSmallerThan :: FileOffset -> FilePath -> IO Bool
isSmallerThan n = fmap ((n >) . fileSize) . getFileStatus

unitFileCheck :: FilePath -> IO Bool
unitFileCheck fp = isSmallerThan (fromIntegral maxSize * 1024 * 1024) fp >>= \b -> do
    if b then hasUnitFiles fp
         else return b

main :: IO ()
main = do
    args <- getArgs

    let base = (args !! 0)
    -- (map (showEither id (Tar.fromTarPath . Tar.entryTarPath)) . getEntries)
    --     <$> readPkgEntries (args !! 0) >>= print

    map (base </>) . filterLzma <$> Tar.getDirectoryContentsRecursive base
        -- >>= filterM (isSmallerThan (10 * 1024 * 1024))
        -- >>= filterM unitFileCheck >>= IO.hPutStrLn IO.stderr . unlines
        >>= mapM (\fp -> unitFileCheck fp >>= \b -> when b $ IO.hPutStrLn IO.stderr fp)

        -- >>= mapM_ (\fp -> hasUnitFiles fp >>= \b -> when b $ IO.hPutStrLn IO.stderr fp)
        -- >>= mapM_ (\fp -> IO.hPutStrLn IO.stderr fp >> readPkgEntries fp)
        -- >>= filterM (\fp -> IO.hPutStrLn IO.stderr fp >> readPkgEntries fp >> return True)

    return ()
