module Common.Functions where

import Control.Monad.Trans.Maybe

infixr 5 </>
(</>) :: String -> String -> String
(</>) s1 s2 = s1 ++ "/" ++ s2

hoistMaybeT :: Monad m => Maybe a -> MaybeT m a
hoistMaybeT = MaybeT . return
