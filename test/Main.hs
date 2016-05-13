module Main where

import Numerical.PETSc.Spec

main :: IO ()
main = 
  tests



-- import Control.Concurrent
-- import Control.Concurrent.Lifted    (fork)
-- import Control.Exception            (Exception, MaskingState (MaskedInterruptible), getMaskingState, throwIO, try)
-- import Control.Exception            (SomeException, handle)

-- import Data.Monoid
-- import Control.Monad                (unless)
-- import Control.Monad.IO.Class       (liftIO)
-- import Control.Monad.Trans.Resource
-- import Data.IORef
-- import Data.Typeable                (Typeable)
-- import Test.Hspec
-- import Test.QuickCheck
-- import Data.Acquire

-- -- * Hspec

-- main :: IO ()
-- main = hspec $ do
--     describe "general" $ do
--         it "survives releasing bottom" $ do
--             x <- newIORef 0
--             handle (\(_ :: SomeException) -> return ()) $ runResourceT $ do
--                 _ <- register $ writeIORef x 1
--                 release undefined
--             x' <- readIORef x
--             x' `shouldBe` 1

-- -- * QuickCheck

-- prop_monoidLaws :: (Monoid a, Show a, Eq a) => Gen a -> Property
-- prop_monoidLaws g = associativity .&&. leftIdent .&&. rightIdent
--     where 
--       associativity = forAll g $ \a -> forAll g $ \b -> forAll g $ \c ->
--                           a <> (b <> c) == (a <> b) <> c
--       leftIdent = forAll g $ \a -> mempty <> a == a
--       rightIdent = forAll g $ \a -> a <> mempty == a
