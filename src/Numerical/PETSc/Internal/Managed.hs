{-# LANGUAGE RankNTypes #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numerical.PETSc.Internal.Managed
-- Copyright   :  (c) Marco Zocca 2015
-- License     :  LGPL3
-- Maintainer  :  Marco Zocca
-- Stability   :  experimental
--
-- | IO resource management abstraction (anything that is acquired using `with-`),
--   from `managed` (by G.Gonzalez)
--
-----------------------------------------------------------------------------
module Numerical.PETSc.Internal.Managed where

import Data.Monoid
import Data.Functor
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class (MonadIO(liftIO))


import System.IO
import Foreign.C.Types

-- newtype Managed a r = Managed {runManaged :: IO a -> (a -> IO r) -> IO r}
newtype Managed a = Managed { (>>-) :: forall r . (a -> IO r) -> IO r}

managed = Managed

withManaged :: forall a b . Managed a -> (a -> IO b) -> IO b
withManaged = (>>-)

runManaged m = m >>- return




-- | instances

instance Functor Managed where
    fmap f mx = Managed (\return_ ->
        mx >>- \x ->
        return_ (f x) )

instance Applicative Managed where
    pure r    = Managed (\return_ ->
        return_ r )

    mf <*> mx = Managed (\return_ ->
        mf >>- \f ->
        mx >>- \x ->
        return_ (f x) )

instance Monad Managed where
    return r = Managed (\return_ ->
        return_ r )

    ma >>= f = Managed (\return_ ->
        ma  >>- \a ->
        f a >>- \b ->
        return_ b )

instance MonadIO Managed where
    liftIO m = Managed (\return_ -> do
        a <- m
        return_ a )

instance Monoid a => Monoid (Managed a) where
    mempty = pure mempty
    mappend = liftA2 mappend







-- ====================================================
-- -- distinct callback return and overall return types:
    
-- newtype Managed2 a =
--   Managed2 { runMng2 :: forall r1 r2 . (a -> IO r1) -> IO r2}
