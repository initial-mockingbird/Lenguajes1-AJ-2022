{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, GADTs        #-}
{-# LANGUAGE KindSignatures, MultiParamTypeClasses, NoImplicitPrelude     #-}
{-# LANGUAGE PolyKinds, TypeFamilies, TypeOperators, UndecidableInstances #-}
--{-# LANGUAGE UndecidableInstances #-}
--{-# LANGUAGE PolyKinds #-}
module TestVector.TestVector where

{-
Genuinamente intente sacar instancias de Arbitrary, pero simplemente no pude construir una funcion:

fromList :: [Double] -> Vector n Double

ya sea usando tipos existenciales, o type families.


-}


import Test.Hspec
import Test.QuickCheck
import Test.Hspec.QuickCheck
import Vector.VectorSafe (Vec(..))
import Vector.Vector 
import qualified GHC.TypeNats as Nats
import qualified GHC.TypeLits as Lits
import           Prelude      hiding (Num (..), (++))
import           Data.Kind    (Type)
import qualified GHC.Num      as N
import Control.Exception (evaluate)

newtype V3G = V3G {getV :: Vector 3} deriving Show


instance Arbitrary V3G where
  arbitrary  = do
    xyz <- vectorOf 3 arbitrary
    let [x,y,z] = xyz 
    return $ V3G $ x :| y :| V1 z

infix 4 ~=

(~=) :: Double -> Double -> Bool
x ~= y = N.abs (x N.- y) <= 0.0000001

v3Spec :: Spec
v3Spec = do 
  describe "Suite de testeo para vectores de tamano 3" $ do
    prop "To list es correcto" $
      \(V3G xs@(x1:|x2:| V1 x3)) -> toList xs == [x1,x2,x3]
    prop "La suma es correcta" $
      \(V3G xs) (V3G ys) -> toList (xs + ys) == zipWith (N.+)  (toList xs) (toList ys) 
    prop "La resta es correcta" $ 
      \(V3G xs) (V3G ys) -> toList (xs - ys) == zipWith (N.-)  (toList xs) (toList ys) 
    prop "La suma por escalar a derecha es correcta" $
      \(V3G xs) n -> toList (xs + n) == map (N.+ n)  (toList xs) 
    prop "La suma por escalar a izquierda es correcta" $
      \(V3G xs) n -> toList (n + xs) == map (n N.+)  (toList xs)
    prop "La multiplicacion por escalar a derecha es correcta" $
      \(V3G xs) n -> toList (xs * n) == map (N.* n)  (toList xs) 
    prop "La multiplicacion por escalar a izquierda es correcta" $
      \(V3G xs) n -> toList (n * xs) == map (n N.*)  (toList xs) 
    prop "El producto interno es correcto entre vectores" $ 
      \(V3G xs) (V3G ys) -> xs % ys ~= sum (zipWith (N.*) (toList xs) (toList ys))
    prop "El producto cruz entre dos vectores cuya dimension no es 3 suelta una excepcion" $
      \x1 x2 y1 y2 -> let
        x :: Vector 2
        x = x1 <:> singleton x2

        y :: Vector 2
        y = y1 <:> singleton y2
        in  evaluate (x*y) `shouldThrow` anyException
    


