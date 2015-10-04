{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
module Numerical.PETSc.Test1 where

-- import Numerical.PETSc.Types
import           Data.Functor ((<$>))
import Foreign
import Control.Monad
-- import           Foreign.C.String (peekCString)
import           Foreign.C.Types
import           Foreign.Marshal.Alloc (alloca)
import           Foreign.Ptr (Ptr)
import System.IO.Unsafe (unsafePerformIO)

import Numerical.PETSc.Internal2
import Language.C.Inline as C

context petscCtx

-- include "<petscksp.h>"
C.include "<petscksp.h>"
C.include "<petsctao.h>"

-- * Vec

vecCreate' comm p = [C.exp|int{VecCreate($(int c), $(Vec *p))} |]
  where c = unComm comm
vecCreate c = withPtr $ vecCreate' c

vecDestroy' p = [C.exp|int{VecDestroy($(Vec *p))}|]
vecDestroy p = with p vecDestroy' 

-- createA :: Const -> IO (Int, Ptr Vec)
-- createA c = alloca $ \p ->
--   create'_  c p >>= \res ->
--     peek p >>= \ptrA ->
--        return (res, ptrA)
-- destroyA c = with c $ destroy'_ >=> \r -> return r



-- * Mat

-- matCreate' c p = [C.exp| int{MatCreate($(int c), $(Mat *p))} |]
matCreate' comm p = [C.exp| int{MatCreate($(int c), $(Mat *p))} |]
  where c = unComm comm 
matDestroy' p = [C.exp|int{MatDestroy($(Mat *p))}|]

matSetFromOptions p = [C.exp| int{MatSetFromOptions($(Mat *p))} |]


-- * KSP

kspCreate' comm p = [C.exp| int{KSPCreate($(int c), $(KSP *p))}|] where
  c = unComm comm
kspCreate c = withPtr $ kspCreate' c

kspDestroy' p = [C.exp| int{KSPDestroy($(KSP *p))}  |]
kspDestroy p = with p kspDestroy'


-- * TAO
taoCreate' comm p = [C.exp| int{TaoCreate($(int c), $(Tao *p))} |] where
  c = unComm comm
taoCreate p = withPtr $ taoCreate' p

taoDestroy' p = [C.exp| int{TaoDestroy($(Tao *p))}  |]
taoDestroy p = with p taoDestroy'

petscInit' = [C.block| int{ PetscInitializeNoArguments();  }|]

petscFin' = [C.block| int{ PetscFinalize(); }|]



{-# NOINLINE commWorld #-}
commWorld = Comm $ unsafePerformIO [C.exp| int{ MPI_COMM_WORLD }  |] 

-- commSelf = liftM Comm $ [C.exp| int{ MPI_COMM_SELF }  |]


-- -- utils

sndM :: Monad m => m (a, b) -> m b
sndM = liftM snd
