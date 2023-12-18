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

-- newtype PaFn p x y = PaFn (p -> x -> y)

-- type PaFn p x y = p -> x -> y

-- class PaFn f where
--     (|====) :: f p y z -> f p' x y -> p -> f p' x z

-- newtype PaFn x y p = PaFn (p -> x -> y)

-- newtype PaFn (f :: p ->  x -> y) = PaFn f

-- (|===) :: PaFn p y z -> PaFn p' x y -> PaFn (p,p') x z
-- (|===) (PaFn f) (PaFn g) = PaFn (\(p,p') -> f p . g p')

type Tensor = Float

newtype PaFn p a = PaFn (p -> a)

runPFn :: PaFn p a -> (p -> a)
runPFn (PaFn f) = f

deparam :: (a -> PaFn p b) -> p -> (a -> b)
deparam f p x = runPFn (f x) p

-- return :: a -> PaFn p a
-- return x = PaFn (const x)



type family Simplify p where
  Simplify () = ()
  Simplify ((), b) = Simplify b
  Simplify (a, ()) = Simplify a
  Simplify (a, b) = (Simplify a, Simplify b)
  Simplify a = a

-- complicate :: forall a . Simplify a -> a
-- complicate = ()

q :: Simplify (((((Integer, (Integer, ())), ()), (Integer, (Integer, ()))), ()), (Integer, (Integer, ())))
q = (((1,1), (1,1)), (1,1))



data Param where
  Empty :: Param
  Param :: a -> Param
  Conc :: a -> b -> Param

qq :: Param
qq = Conc True (Conc () 17)
-- type family Marap m where


(>>=) :: PaFn p a -> (a -> PaFn p' b) -> PaFn (p, p') b
pfn >>= f = PaFn (\(p, p') -> runPFn (f (runPFn pfn p)) p')

(>>) :: PaFn p a -> PaFn p' b -> PaFn (p, p') b
pfn >> f = pfn >>= const f

(>=>) :: (a -> PaFn p b) -> (b -> PaFn p' c) -> (a -> PaFn (p, p') c)
(f >=> g) x = f x >>= g

param :: PaFn p p
param = PaFn id

return :: a -> PaFn () a
return x = PaFn (const x)

-- layer1 :: b -> PaFn p (p, b)
-- layer1 x = PaFn (\p -> (p, x))

-- layer2 :: (Num a, Ord a) => a -> PaFn () a
-- layer2 x = PaFn (\() -> relu x)

-- nn :: (Num (p, b), Ord p, Ord b) => b -> PaFn (p, ((), p')) (p, b)
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

mlp1 = lin >=> relu >=> lin >=> relu >=> lin

-- chain [pfn] = pfn
-- chain (pfn : pfns) = pfn >=> (chain pfns)

-- data Param where
--   Empty :: Param
--   Param :: a -> Param
--   Conc :: a -> b -> Param

-- conc :: Param -> Param -> Param
-- conc x Empty = x
-- conc Empty y = y
-- conc x y = Conc x y

-- q :: Param (a, b)
-- q x y = (x, y)

-- data instance Param () = Empty
-- data instance Param (p, p') = Param p p'

-- q :: Param p -> Param p'
-- q Empty = Empty

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

