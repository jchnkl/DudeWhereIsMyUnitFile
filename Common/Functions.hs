module Common.Functions where

import Data.List (isInfixOf, isSuffixOf)
import Control.Monad.Trans.Maybe

infixr 5 </>
(</>) :: String -> String -> String
(</>) s1 s2 = s1 ++ "/" ++ s2

hoistMaybeT :: Monad m => Maybe a -> MaybeT m a
hoistMaybeT = MaybeT . return

isUnitFile :: FilePath -> Bool
isUnitFile fp = isInfixOf "usr/lib/systemd" fp && isSuffixOf ".service" fp

hasUnitFiles :: [FilePath] -> Bool
hasUnitFiles = foldr (||) False . map isUnitFile
