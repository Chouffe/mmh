{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE LambdaCase       #-}

module Eff.Utils
    (runNat)
    where

import Control.Monad.Freer (Member, Eff, send)
import Control.Monad.Freer.Internal (handleRelay)

runNat
    :: Member m effs
    => (forall a . eff a -> m a)
    -> Eff (eff ': effs) b
    -> Eff effs b
runNat f = handleRelay pure $ \e -> (send (f e) >>=)
