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
module TestVectorSafe.TestVectorSafe where

{-
Genuinamente intente sacar instancias de Arbitrary, pero simplemente no pude construir una funcion:

fromList :: [Double] -> Vector n Double

ya sea usando tipos existenciales, o type families.


-}


import Test.Hspec
import Test.QuickCheck
import Vector.Vector
import qualified GHC.TypeNats as Nats
import qualified GHC.TypeLits as Lits
import Prelude hiding ((++))
import           Data.Kind    (Type)
import Data.Singletons
type family (n :: Nat) + (m :: Nat) :: Nat where
    'Z   + m = m
    'S n + m = 'S (n + m)

(++) :: Vec n a -> Vec m a -> Vec (n + m) a
(++) (V1 x) ys    = x :| ys
(++) (x :| xs) ys = x :| (xs ++ ys)


data SNat (n :: Nat) where
  SZ :: SNat Z
  SS :: SNat n -> SNat (S n)

data SomeSL (a :: *) where
    MkSomeSL :: Lits.KnownNat n => SNat n -> Vec n a -> SomeSL a  

