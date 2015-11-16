{-# LANGUAGE RankNTypes, FlexibleInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numerical.PETSc.Internal.Managed
-- Copyright   :  (c) Marco Zocca 2015
-- License     :  LGPL3
-- Maintainer  :  zocca . marco . gmail . com
-- Stability   :  experimental
--
-- | IO resource management abstraction (anything that is acquired using `with-`),
--   taken from the `managed` package
--
-----------------------------------------------------------------------------
module Numerical.PETSc.Internal.Managed
       -- (Managed, runManaged, managed, withManaged)
       where

-- import Data.Monoid
-- import Data.Functor
-- import Control.Applicative
-- import Control.Monad
-- import Control.Monad.IO.Class (MonadIO(liftIO))

-- import System.IO
-- import Foreign.C.Types

import Control.Monad.Managed

import Control.Monad.Trans
import Control.Monad

newtype ManagedT m a = ManagedT {runManagedT :: forall r . (a -> m r) -> m r}

instance MonadTrans ManagedT where


-- -- |

-- newtype Managed a = Managed { (>>-) :: forall r . (a -> IO r) -> IO r}

-- managed :: forall a. (forall r. (a -> IO r) -> IO r) -> Managed a
-- managed = Managed

-- withManaged :: forall a r . Managed a -> (a -> IO r) -> IO r
-- withManaged = (>>-)

-- runManaged :: forall r . Managed r -> IO r
-- runManaged m = m >>- return







-- -- | instances

-- instance Functor Managed where
--     fmap f mx = Managed (\return_ ->
--         mx >>- \x ->
--         return_ (f x) )

-- instance Applicative Managed where
--     pure r    = Managed (\return_ ->
--         return_ r )

--     mf <*> mx = Managed (\return_ ->
--         mf >>- \f ->
--         mx >>- \x ->
--         return_ (f x) )

-- instance Monad Managed where
--     return r = Managed (\return_ ->
--         return_ r )

--     ma >>= f = Managed (\return_ ->
--         ma  >>- \a ->
--         f a >>- \b ->
--         return_ b )

-- instance MonadIO Managed where
--     liftIO m = Managed (\return_ -> do
--         a <- m
--         return_ a )

-- instance Monoid a => Monoid (Managed a) where
--     mempty = pure mempty
--     mappend = liftA2 mappend





-- fmapMng :: forall a b. (a -> b) -> Managed a -> Managed b
-- fmapMng f mx = managed $ \ret ->
--   mx >>- \x -> ret (f x)


-- apMng :: forall a b. Managed (a -> b) -> Managed a -> Managed b
-- apMng mf mx = managed $ \ret ->
--   mf >>- \f ->
--   mx >>- \x ->
--   ret (f x)

-- bindMng :: forall a b. Managed a -> (a -> Managed b) -> Managed b
-- bindMng m f = managed $ \ret ->
--   m >>- \a ->
--   f a >>- \b ->
--   ret b





