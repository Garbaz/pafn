{-# HLINT ignore "Use tuple-section" #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

import Prelude hiding (return, (>>), (>>=))

-- (|>) :: (y -> z) -> (x -> y) -> x -> z
-- (|>) f g x = f (g x)

-- (>=>) :: (p -> x -> y) -> (p' -> y -> z) -> (p, p') -> x -> z
-- (>=>) f g (p, p') = g p' . f p

-- (|===) :: (p -> y -> z) -> (p' -> x -> y) -> p -> (p' -> x -> z)
-- (|===) f g p p' = f p . g p'

newtype Pid p = Pid p

newtype Fid x y = Fid (x -> y)

-- newtype PFn p x y = PFn (p -> x -> y)

-- type PFn p x y = p -> x -> y

-- class PFn f where
--     (|====) :: f p y z -> f p' x y -> p -> f p' x z

-- newtype PFn x y p = PFn (p -> x -> y)

-- newtype PFn (f :: p ->  x -> y) = PFn f

-- (|===) :: PFn p y z -> PFn p' x y -> PFn (p,p') x z
-- (|===) (PFn f) (PFn g) = PFn (\(p,p') -> f p . g p')

type Tensor = Float

newtype PFn p a = PFn (p -> a)

runPFn :: PFn p a -> (p -> a)
runPFn (PFn f) = f

deparam :: (a -> PFn p b) -> p -> (a -> b)
deparam f p x = runPFn (f x) p

-- return :: a -> PFn p a
-- return x = PFn (const x)

(>>=) :: PFn p a -> (a -> PFn p' b) -> PFn (p, p') b
pfn >>= f = PFn (\(p, p') -> runPFn (f (runPFn pfn p)) p')

(>>) :: PFn p a -> PFn p' b -> PFn (p, p') b
pfn >> f = pfn >>= const f

(>=>) :: (a -> PFn p b) -> (b -> PFn p' c) -> (a -> PFn (p, p') c)
(f >=> g) x = f x >>= g

param :: PFn p p
param = PFn id

return :: a -> PFn () a
return x = PFn (const x)

-- layer1 :: b -> PFn p (p, b)
-- layer1 x = PFn (\p -> (p, x))

-- layer2 :: (Num a, Ord a) => a -> PFn () a
-- layer2 x = PFn (\() -> relu x)

-- nn :: (Num (p, b), Ord p, Ord b) => b -> PFn (p, ((), p')) (p, b)
-- nn x = do
--   x <- layer1 x
--   x <- layer2 x
--   return (relu x)

lin x = do
  w <- param
  b <- param
  return (w * x + b)

relu x = return (relu' x)
  where
    relu' x | x > 0 = x
    relu' _ = 0

mlp1 :: Tensor -> PFn (((((Tensor, (Tensor, ())), ()), (Tensor, (Tensor, ()))), ()), (Tensor, (Tensor, ()))) Tensor
mlp1 = lin >=> relu >=> lin >=> relu >=> lin

chain [pfn] = pfn
chain (pfn : pfns) = pfn >=> (chain pfns)

data Param where
  Empty :: Param
  Param :: a -> Param
  Conc :: a -> b -> Param

conc :: Param -> Param -> Param
conc x Empty = x
conc Empty y = y
conc x y = Conc x y

-- data family Param a

-- data instance Param b () = Single b
-- data instance Param () = Empty
-- newtype instance Param (x,()) = Left x
-- newtype instance Param ((), y) = Right y
-- newtype instance Param (x,y) = Conc (x,y)

-- red Empty = Empty
-- red (Single ()) = Empty
-- red (Conc x ()) = Single x
-- red (Conc () y) = Single y
-- red p = p

-- newtype Pair x y = Pair x y

-- data instance Param (Pair x ()) = x
-- data instance Param ((), y) = y
-- data instance Param (x, y) = (x, y)

-- red (Param ((), ())) = ()
-- red (Param (x, ())) = x
-- red (Param ((), y)) = y
-- red (Param (x, y)) = (x,y)

-- -- type Matrix3x3 = Tensor (3,3)

-- type Vector (n :: Int) = ()

-- concat :: Vector m -> Vector n -> Vector (m + n)
