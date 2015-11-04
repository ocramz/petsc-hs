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
--   taken from the `managed` package
--
-----------------------------------------------------------------------------
module Numerical.PETSc.Internal.Managed
       (Managed, runManaged, managed, withManaged)
       where

import Data.Monoid
import Data.Functor
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class (MonadIO(liftIO))

import System.IO
-- import Foreign.C.Types

newtype Managed a = Managed { (>>-) :: forall r . (a -> IO r) -> IO r}

managed :: forall a. (forall r. (a -> IO r) -> IO r) -> Managed a
managed = Managed

withManaged :: forall a r . Managed a -> (a -> IO r) -> IO r
withManaged = (>>-)

runManaged :: forall r . Managed r -> IO r
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





{- $reexports
    "Control.Monad.IO.Class" re-exports 'MonadIO'
-}
