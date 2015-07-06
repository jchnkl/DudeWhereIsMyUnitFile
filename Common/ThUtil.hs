module Common.ThUtil where

import Data.Char (toLower)
import Data.List (isPrefixOf)

dropPrefix :: String -> String -> String
dropPrefix pre str | isPrefixOf pre str = drop (length pre) str
                   | otherwise          = str

sanitizeRecordName :: Int -> String -> String
sanitizeRecordName n str = let fn = drop n str in toLower (head fn) : tail fn
