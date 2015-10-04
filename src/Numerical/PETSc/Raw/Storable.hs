module Numerical.PETSc.Raw.Storable where

import Foreign
import Foreign.C
import Foreign.Storable
import Control.Monad

-- class Storable a => PetscStorable a where
-- -- -- -- abstract methods for resp.: with-, destructive update
--   withPStorable :: a -> (Ptr a -> IO b) -> IO b

  



  
-- withSizeArray :: (Storable a, Integral a) => [a] -> (Ptr CULong -> IO b) -> IO b
-- withSizeArray = withArray . liftM fromIntegral
