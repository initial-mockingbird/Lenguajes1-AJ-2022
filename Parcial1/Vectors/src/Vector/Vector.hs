{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies  #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

{-
Long live these guys!

Vector implementation
https://blog.jle.im/entry/fixed-length-vector-types-in-haskell.html


More interesting details here!!! (can't wait to learn unification from prolog!)
https://programmable.computer/posts/datakinds_runtime.html#fnref1


it was a TOTAL NIGHTMARE implementing the vector class due to the SUPREME OVERLOADING OF *: it can return either a vector OR a scalar.

So what do we do? we create a type family with a phantom type, credits to:

https://stackoverflow.com/questions/59542658/how-to-overload-operator-with-type-synonyms


We CAN'T generalize instances further since:
                            
instance (KnownNat dim, Num a) => SuperVector (ListVector dim a) a where
                                                                 ^
                Compiler will interpret this as: anything that has the form a: [a], Maybe a, Either String, Will all match!
                (Also, compiler won't check the Num a >:( ).

and since we are actually defining a type family, every output type should have been defined inside the class... That means
we need two inner types: one for Vector and One for Scalars (and double the troubles!)

-}

module Vector.Vector 
    ( fromList
    , SuperVector((+),(-),(*))
    , (%)
    , V3
    , V2
    , Vector) where

import Data.List (intercalate)
import GHC.TypeNats
import Data.Proxy
import Data.Kind (Type)


newtype ListVector (dim :: Nat) a = Ve {getV :: [a]}


type V3     = ListVector 3
type V2     = ListVector 2
type Vector = ListVector

class SuperVector v a  where
    type V v a
    type S v a 
    infixl 6 +
    infixl 6 -
    infixl 7 *
    (+) :: v -> a -> V v a
    (-) :: v -> a -> V v a
    (*) :: v -> a -> V v a -- cross product/
    -- We need a proxy since neither type variable (v/a)  uniquely determines the other....
    -- since we are working with type families, the result should always be a type? returning 'a' yields an error....
    f   :: a -> v -> v -> S v a



instance (KnownNat dim) => SuperVector (ListVector dim Double) Double where
    type V (ListVector dim Double) Double = (ListVector dim Double)
    type S (ListVector dim Double) Double = Double
    Ve v + a = Ve $ map (Prelude.+a) v
    Ve v - a = Ve $ map (Prelude.-a) v
    Ve v * a = Ve $ map (Prelude.*a) v
    f _ _ = error "Cannot do a Dot product between a scalar and a vector"




instance (KnownNat dim) => SuperVector (ListVector dim Double) (ListVector dim Double) where
    type V (ListVector dim Double) (ListVector dim Double) = (ListVector dim Double)
    type S (ListVector dim Double) (ListVector dim Double) = Double
    Ve v + Ve v' = Ve $ zipWith (Prelude.+) v v'
    Ve v - Ve v' = Ve $ zipWith (Prelude.-) v v'
    Ve v * Ve v' 
        | d == 3    = Ve [p a2 b3 `m` p a3 b2,p a1 b3 `m` p a3 b1, p a1 b2 `m` p a2 b1]
        | otherwise = error "Cross product only known for 3 dimensions!"
        where
            d = fromIntegral (natVal (Proxy @dim))
            [a1,a2,a3] = v
            [b1,b2,b3] = v'
            p = (Prelude.*)
            m = (Prelude.-)
    f _ (Ve v)  (Ve v') = sum (zipWith (Prelude.*) v v')


instance (KnownNat dim) => SuperVector Double (ListVector dim Double) where
    type V Double (ListVector dim Double) = (ListVector dim Double)
    type S Double (ListVector dim Double) = Double
    a + Ve v = Ve $ map (a Prelude.+) v
    a - Ve v = Ve $ map (a Prelude.-) v
    a * Ve v = Ve $ map (a Prelude.*) v
    f _ _ _ = error "Cannot do a Dot product between a scalar and a vector"


instance (Show a) => Show (ListVector dim a) where
    show (Ve v) = '(' : intercalate ","  (map show v) ++ ")"


fromList :: forall dim . (KnownNat dim) => [Double] -> ListVector dim Double
fromList xs 
    | length xs == d = Ve {getV=xs} 
    | otherwise      = error "Bad Dimention"
    where
        d = fromIntegral (natVal (Proxy @dim))

infixl 7 %
(%) :: KnownNat dim => ListVector dim Double -> ListVector dim Double -> Double
v % v' = f' 0 v v' 
    where 
        f' :: SuperVector a Double => Double -> a -> a -> S a Double
        f' = f 

main' :: IO ()
main' = print v4
    where
        v3 :: V3 Double
        v3 = fromList [1,2,3]
        v3P1 =  v3 Vector.Vector.+ (3.0 :: Double)
        v4 = v3P1 Vector.Vector.+ v3
