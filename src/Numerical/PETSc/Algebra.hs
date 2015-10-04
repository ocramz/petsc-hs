{-# LANGUAGE ConstraintKinds #-}
module Numerical.PETSc.Algebra where
{- some notes on possible algebraic categorization
   i.e. "improving Num"
-}

-------- see also J. Hinkle's "diffgeom"



-------- (Olaf Klinke)

-- class Semigroup a where
--   (<>) :: a -> a -> a

-- class Group a where
--   (*) :: a -> a -> a
--   unit :: a
--   inverse :: a -> a

-- superclass Semigroup Group where
--   (<>) = (*)

-------- (M.Farkas-Dyck)

class Semigroup a where
    (<>) :: a -> a -> a

newtype Sum a = Sum a

newtype Product a = Product a

-- (+.) :: Semigroup (Sum a) => a -> a -> a
-- (*.) :: Semigroup (Product a) => a -> a -> a

class Semigroup a => Abelian a

-- with ConstraintKinds
type Semiring a = (Abelian (Sum a), Semigroup (Product a))
