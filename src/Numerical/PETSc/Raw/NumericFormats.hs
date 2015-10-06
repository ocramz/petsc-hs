{-# LANGUAGE TypeFamilies, MultiParamTypeClasses #-}
module NumericFormats where

import Storable
import Data.Vector
import Data.Complex


-- -- some ideas from HMatrix

-- | indexing into numerical containers (see Data.Packed.Internal.Numeric)

type family IndexOf (c :: * -> *)
type instance IndexOf Vector = Int
type instance IndexOf Matrix = (Int, Int)


-- | complex numbers

class Complexable c where
  toComplex :: RealElement e => (c e, c e) -> c (Complex e)
  fromComplex :: RealElement e => c (Complex e) -> (c e, c e)
  -- comp :: RealElement e => c e -> c (Complex e) -- I don't know what this is for

-- -- 
-- instance Complexable Vector where
-- instance Complexable Matrix where -- .. etc.

  

-- | a generic element class 
-- -- -- (see Data.Packed.Internal.Matrix)
  
-- class (Storable a) => Element a where
  

-- | a generic container class
-- -- -- unifying operations over vectors and matrices
-- -- -- (inspired by Data.Packed.Internal.Numeric in HMatrix )

class (Complexable c, Fractional e, Element e) => Container c e
  where
    containerSize :: c e -> IndexOf c
    containerScalar :: e -> c e
    containerConj :: c e -> c e
    containerScale :: e -> c e -> c e
    containerScaleRecip :: e -> c e -> c e
    containerAddConst :: e -> c e -> c e
    containerAdd :: c e -> c e -> c e -- elementwise operations
    containerSub :: c e -> c e -> c e
    containerMul :: c e -> c e -> c e
    containerDiv :: c e -> c e -> c e
    containerEqual :: c e -> c e -> Bool
    containerMap :: (Element b) => (e -> b) -> c e -> c b
    containerConst :: e -> IndexOf c -> c e

    containerAtIdx :: c e -> IndexOf c -> e
    
  
  
