module Numerical.PETSc.Main where

import Numerical.PETSc.Internal
import Control.Monad (unless, when)
-- import Numerical.PETSc.Types


main :: IO ()
main = 
  return ()

withPetsc f = do
  ei <- petscInit'
  f
  putStrLn $ "so far so good" ++ show ei
  petscFin'

-- withPtr_ f = do
--   (x, ()) <- withPtr f
--   return x

-- withPtr_ :: (Storable a) => (Ptr a -> IO ()) -> IO a
-- withPtr f = do
--   alloca $ \ptr -> do
--     x <- f ptr
--     y <- peek ptr
--     return (y, x)
