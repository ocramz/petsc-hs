{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
module Numerical.PETSc.Internal where

import Numerical.PETSc.Types
import qualified Language.C.Inline as C
import qualified Language.C.Types as CT
import Foreign.C.Types
import Foreign.C.String
import Foreign
import qualified Language.Haskell.TH as TH
import Language.C.Inline.Context
import Data.Monoid ((<>), mempty)
import Control.Monad (unless, when, liftM)
import qualified Data.Map as Map
import Language.C.Inline

import qualified Data.Vector.Storable.Mutable as V

C.include "<stdio.h>"
C.include "<math.h>"

C.include "<petscksp.h>"
C.include "<petscsys.h>"

-- * inline-c PETSc Context (type mappings)
petscCtx :: Context
petscCtx = baseCtx <> funCtx <> vecCtx <> ctx where
  ctx = mempty {ctxTypesTable = petscTypesTable}

petscTypesTable :: Map.Map CT.TypeSpecifier TH.TypeQ
petscTypesTable = Map.fromList
                  [
                    (typeNameId "PetscInt", [t| PetscInt_ |]) 
                  , (typeNameId "PetscError", [t| PetscError_ |]) 
                  , (typeNameId "PetscBool", [t| PetscBool_ |])
                  , (typeNameId "Vec", [t| Vec |] )
                  ]

typeNameId :: String -> CT.TypeSpecifier
typeNameId = CT.TypeName . CT.Identifier

-- * type synonyms

type PetscInt_ = CInt
type PetscError_ = CInt
type PetscBool_ = Bool

-- * newtypes

-- data Vec = Ptr ()
-- instance Storable Vec 

-- instance Storable Vec where
--   sizeOf _ = sizeOf (undefined :: Ptr ())
--   alignment _ = alignment (undefined :: Ptr ())
--   peek _ = error "unimplemented"
--   poke _ _ = error "unimplemented"

newtype Vec = Vec (Ptr Vec)
deriving instance Storable Vec




-- * utilities

boolErr :: (Monad m , Num a, Eq a) => m a -> m Bool
boolErr = liftM toBool

sndM :: Monad m => m (a, b) -> m b
sndM = liftM snd


-- * Low-level hooks

-- PETSC_EXTERN PetscErrorCode PetscInitialize(int*,char***,const char[],const char[]);
-- petscInitOpts args = [C.block| int{ PetscInitialize( $vec-len:args, $vec-ptr:(string **args));}|] where
--   argc = length args

-- PETSC_EXTERN PetscErrorCode PetscInitializeNoArguments(void);
petscInit' = [C.block| int{
                 PetscInt e;
                 e = PetscInitializeNoArguments(); CHKERRQ(e); }|]

petscFin' = [C.block| int{ PetscFinalize(); }|]

-- petscInitq = boolErr [C.block| int{ PetscBool *b; PetscInt e;
--                          e  = PetscInitialized( &b);} |]

-- petscFinq = boolErr [C.block| int{ PetscBool *b; PetscInt e;
--                                  e = PetscFinalized( &b);} |]

-- petscInit = do -- no options file and helpstring
--   fin <- petscFinq
--   unless fin c where
--     c = do
--       petscInit'
--       putStrLn $ "PETSc initialized"

-- petscFin = do
--   init <- petscInitq
--   when init c1 where
--     c1 = do
--       petscFin'
--       putStrLn "PETSc finalized"

-- * misc

f1 = do
  let n = 10
  vec <- V.replicate (fromIntegral n) 3
  V.unsafeWith vec $ \ptr -> [C.block| int {
        int i;
        int x = 0;
        for (i = 0; i < $(int n); i++) {
          x += $(int *ptr)[i];
        }
        return x;
      } |]



-- * Vec

-- create :: IO (Ptr ())
-- create = [C.exp| void * { create() } |]

-- push :: Ptr () -> C.CInt -> IO ()
-- push s i = [C.exp| void { push($(void *s), $(int i)) } |]

-- pop :: Ptr () -> IO C.CInt
-- pop s = [C.exp| int { pop($(void *s)) } |]

-- vc1 :: Comm -> IO (Ptr ())
-- vc1 comm =
--   [C.exp| Vec{}|]
--   -- return v
--    where
--     c = unComm comm


-- initNagError :: (Ptr NagError -> IO a) -> IO a
-- initNagError f = alloca $ \ptr -> do
--   [exp| void{ INIT_FAIL(*$(NagError *ptr)) } |]
--   f ptr

-- PETSC_EXTERN PetscErrorCode VecCreate(MPI_Comm,Vec*); 
-- vecCreate' :: Comm -> IO (Vec, PetscError_)
-- vecCreate' comm = withPtr $ \x -> [C.block| int{ PetscErrorCode e; Vec v;  
--                                      e = VecCreate( $(int c), &v);
--                                      CHKERRQ(e); 
--                                               }|]
--                                         where c = unComm comm 
-- withPtr' f = 
--   alloca $ \ptr -> f ptr







-- * errors

-- checkPetscError ptr f = do
--   x <- f
--   e <- [C.exp| int{ $(PetscError *ptr) }|]

-- chke n = [C.exp| int{ CHKERRQ( $(int n) )}|]


-- * MPI stuff

data Comm = Comm {unComm :: CInt} deriving (Eq, Show)

commWorld = liftM Comm $ [C.exp| int{ MPI_COMM_WORLD }  |] 
commSelf = liftM Comm $ [C.exp| int{ MPI_COMM_SELF }  |]




-- test1 = do
--   -- i <- [C.exp| int{ printf( "Input %d integers: ", $(int n))}]
--   s <- readAndSum n
--   [C.exp| void{ printf( "Sum: %.2d\n", $(int s)) } |]
--   putStrLn $ "Sum: " ++ show s


-- test1 :: IO ()
-- test1 = do
--    x <- [C.exp| int{ printf("Some number: %.2f\n", cos(0.5)) } |]
--    putStrLn $ show x ++ " characters printed."

-- readAndSum :: CInt -> IO CInt
-- readAndSum n = 
--   [C.block| int {
--       int i, sum = 0, tmp = 0;
--       for (i = 0; i < $(int n); i++) {
--         scanf("%d ", &tmp);
--         sum += tmp;
--       }
--       return sum;
--     } |]
