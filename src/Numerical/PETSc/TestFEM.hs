module Numerical.PETSc.TestFEM where

import Numerical.PETSc.Test2

import Foreign.C.Types
import Control.Monad
import Control.Concurrent
import Control.Exception
import Foreign

import qualified Data.Vector as V



t11' h m n =
  withMatAssembleQ4 cs m n (ke h) $ \mat ->
    matViewStdout mat
   where cs = commWorld

t11 = withPetsc0 $ t11' 2.0 10 11


withMatAssembleQ4 comm m n localmat
  | length localmat == 16 =
      withMatPipeline comm nNodes nNodes (\mat -> assembleQ4 mat m n localmat)
  | otherwise = error "withMatAssembleQ4: localmat must be length 16"
  where
    nnrow = m+1
    nncol = n+1
    nNodes = nnrow * nncol


assembleQ4 mat m n localmat =
  forM_ [0 .. nElem] (\j -> matSetValues mat (idx j) (idx j) localmat AddValues)
   where
    nElem = m * n
    idx j = indices4 j m

indices4 i m = [idx0, idx1, idx2, idx3] where
      idx0 = (m+1)*(i `div` m) + (i `mod` m)
      idx1 = idx0 + 1
      idx2 = idx1 + m + 1
      idx3 = idx1 + m

ke0 :: Fractional a => [a]
ke0 = [1/6, -1/8, 1/12, -1/8,
      -1/8, 1/6, -1/8, 1/12,
      1/12, -1/8, 1/6, -1/8,
      -1/8, 1/12, -1/8, 1/6] 

ke h = map (*h) ke0
