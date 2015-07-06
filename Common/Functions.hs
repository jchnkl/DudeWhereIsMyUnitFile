module Common.Functions where

import Control.Monad.Trans.Maybe

hoistMaybeT :: Monad m => Maybe a -> MaybeT m a
hoistMaybeT = MaybeT . return
