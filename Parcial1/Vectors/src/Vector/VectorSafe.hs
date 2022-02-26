{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Vector.VectorSafe where

import           Data.Kind    (Type)
import           Data.Proxy   (Proxy (..))
import qualified GHC.Num      as N
import qualified Data.List ((!!))
import           GHC.TypeNats 
import           Prelude      hiding (Num (..))
import GHC.Base (undefined)
import Data.Singletons (unSingFun2)
import Data.Finite

data Vec (n :: Nat) a  where
    V1   :: a -> Vec 1 a
    (:|) :: (2 <= n ) => a -> Vec (n-1) a -> Vec n a

infixr 5 :|
infixr 5 <:>

{- Sendo caso borde:
a :: Vector 0
a = undefined

b = 1 <:> a <- hidden undefined hace que esto se vuelva un vector 1
-}
(<:>) :: ((<=?) 2  n ~ True) => Double  -> Vector (n-1) -> Vector n
(<:>) = (:|)

singleton :: Double -> Vector 1
singleton = V1

toList :: Vector n -> [Double]
toList (V1 x) = [x]
toList (x :| xs) = x : toList xs

type V2 = Vector 2
type V3 = Vector 3
type Vector (n :: Nat) = Vec n Double

instance Show a => Show (Vec n a) where
    show xs = '<' : show' xs  ++ ">"
        where
            show' :: forall n a. Show a => Vec n a -> String
            show' (V1 x)    = show x
            show' (x :| xs) = show x ++ ',' : show' xs

class SuperVector v a where
    type V v a
    type S v a
    infixl 6 +
    infixl 6 -
    infixl 7 *
    -- since we are working with type families, the result should always be a type? returning 'a' yields an error....
    (+) :: v -> a -> V v a
    (-) :: v -> a -> V v a
    (*) :: v -> a -> V v a -- cross product/
    -- We need a proxy since neither type variable (v/a)  uniquely determines the other....
    f   :: Proxy a -> v -> v -> S v a

index :: Vector n -> Finite n -> Double
index v i = toList v !! fromIntegral (getFinite i)


instance SuperVector (Vec n Double) (Vec n Double) where
    type V (Vec n Double) (Vec n Double) = Vec n Double
    type S (Vec n Double) (Vec n Double) = Double

    (V1 x)    + (V1 y)    = V1 $ (N.+) x y
    (x :| xs) + (y :| ys) = (N.+) x y :| (xs + ys)
    _ + _                 = undefined

    (V1 x)    - (V1 y)    = V1 $ (N.-) x y
    (x :| xs) - (y :| ys) = (N.-) x y :| (xs - ys)
    _ - _                 = undefined

    (a1 :| a2 :| V1 a3) * (b1 :| b2 :| V1 b3) = x1 :| x2 :| V1 x3
        where
            p  = (N.*)
            m  = (N.-)
            x1 = p a2 b3 `m` p a3 b2
            x2 = p a1 b3 `m` p a3 b1
            x3 = p a1 b2 `m` p a2 b1
    _ * _ = undefined

    f _ (V1 x)     (V1 y)    = (N.*) x y
    f _ (x :| xs) (y :| ys)  = (N.+) ((N.*) x y) (f a xs ys)
        where
            a :: Proxy (Vec (n-1) Double)
            a = Proxy
    f _ _  _                 = undefined

instance SuperVector (Vec n Double) Double where
    type V (Vec n Double) Double = (Vec n Double)
    type S (Vec n Double) Double = Double

    (x :| xs) + n = (N.+) x n :| (xs + n)
    V1 x + n      = V1 $ (N.+) x n

    (x :| xs) - n = (N.-) x n :| (xs - n)
    V1 x - n      = V1 $ (N.-) x n

    (x :| xs) * n = (N.*) x n :| (xs * n)
    V1 x * n      = V1 $ (N.*) x n

    f _ _ = error "Cannot do a Dot product between a scalar and a vector"


instance SuperVector Double (Vec n Double) where
    type V Double (Vec n Double) = (Vec n Double)
    type S Double (Vec n Double) = Double

    n + (x :| xs) = (N.+) n x :| n + xs
    n + V1 x      = V1 $ (N.+) n x

    n - (x :| xs) = (N.-) n x :| (n - xs)
    n - V1 x      = V1 $ (N.-) n x

    n * (x :| xs) = (N.*) n x :| n * xs
    n * V1 x      = V1 $ (N.*) n x

    f _ _ = error "Cannot do a Dot product between a scalar and a vector"


infixl 7 %
(%) ::  Vector dim -> Vector dim -> Double
v % v' = f' Proxy v v'
    where
        f' :: Proxy (Vector dim) -> Vector dim  -> Vector dim  -> S (Vector dim)  Double
        f' = f
