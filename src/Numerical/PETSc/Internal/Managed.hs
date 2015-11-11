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
       (Managed, runManaged, managed, withManaged)
       where

import Data.Monoid
import Data.Functor
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class (MonadIO(liftIO))

import System.IO
-- import Foreign.C.Types


-- | a `Manageable` typeclass ?

-- class Mng a where
--   mng :: forall r . (a -> IO r) -> IO r



-- |

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





fmapMng :: forall a b. (a -> b) -> Managed a -> Managed b
fmapMng f mx = managed $ \ret ->
  mx >>- \x -> ret (f x)


apMng :: forall a b. Managed (a -> b) -> Managed a -> Managed b
apMng mf mx = managed $ \ret ->
  mf >>- \f ->
  mx >>- \x ->
  ret (f x)

bindMng :: forall a b. Managed a -> (a -> Managed b) -> Managed b
bindMng m f = managed $ \ret ->
  m >>- \a ->
  f a >>- \b ->
  ret b





-- -- ====================================================
-- -- -- distinct callback return and overall return types:
    
-- newtype Managed2 a =
--   Managed2 { runMng2 :: forall r1 r2 . (a -> IO r1) -> IO r2}

-- (>>||) = runMng2
-- mng2 = Managed2

-- bindMng2 :: forall a b. Managed2 a -> (a -> Managed2 b) -> Managed2 b
-- bindMng2 m f = mng2 $ \ret ->
--   m >>|| \a ->
--   f a >>|| \b ->
--   ret b

-- apMng2 :: forall a b. Managed2 (a -> b) -> Managed2 a -> Managed2 b
-- apMng2 mf mx = mng2 $ \ret -> mf >>|| \f -> mx >>|| \x -> ret (f x)

-- fmapMng2 f mx = managed $ \ret -> mx >>|| \x -> ret $ f x


-- -- | Managed2 is not a monad (fmap and ap work though); `return` and `pure` don't typecheck, hence the partial instances below. 


-- instance Functor Managed2 where
--     fmap f mx = Managed2 (\return_ ->
--         mx >>|| \x ->
--         return_ (f x) )

-- instance Applicative Managed2 where
--     -- pure r    = Managed2 (\return_ ->
--     --     return_ r )

--     mf <*> mx = Managed2 (\return_ ->
--         mf >>|| \f ->
--         mx >>|| \x ->
--         return_ (f x) )

-- instance Monad Managed2 where
--     -- return r = Managed2 (\return_ ->
--     --     return_ r )

--     ma >>= f = Managed2 (\return_ ->
--         ma  >>|| \a ->
--         f a >>|| \b ->
--         return_ b )

-- -- instance MonadIO Managed2 where
-- --     liftIO m = Managed2 (\return_ -> do
-- --         a <- m
-- --         return_ a )

-- instance Monoid a => Monoid (Managed2 a) where
--     mempty = pure mempty
--     mappend = liftA2 mappend



{- $reexports
    "Control.Monad.IO.Class" re-exports 'MonadIO'
-}
