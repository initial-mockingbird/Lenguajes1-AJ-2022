module Church where

import Prelude


data Church = Cero | Suc Church

suma :: Church -> Church -> Church 
suma Cero n   = n
suma (Suc n) n' = suma n (Suc n')

multiplicacion :: Church -> Church -> Church 
multiplicacion Cero _ = Cero 
multiplicacion _ Cero = Cero
multiplicacion (Suc Cero) n = n 
multiplicacion n (Suc Cero) = n 
multiplicacion (Suc n) n'   = suma n' $ multiplicacion n n' 

toInt :: Church -> Int
toInt Cero = 0
toInt (Suc n) = 1 + toInt n


type ChurchT = (forall a. (a -> a) -> (a -> a))

-- hay alguna ventaja de usar esta definicion????
-- type ChurchT2 a = (a -> a) -> (a -> a)

-- Lambda calculus style.

zeroT :: ChurchT
zeroT = (\_ -> (\x -> x))

succ :: ChurchT -> ChurchT
succ = \n -> \f -> \x -> f (n f x)

plus :: ChurchT -> ChurchT -> ChurchT
plus = \m -> \n -> \f -> \x -> m f (n f x)

mult :: ChurchT -> ChurchT -> ChurchT
mult = \m -> \n -> \f -> \x -> m (n f) x

-- Since numerals are just functions

zeroT' :: ChurchT
zeroT' = const identity

plus' :: ChurchT -> ChurchT -> ChurchT
plus' =  (<<<)

toInt' :: ChurchT -> Int
toInt' f = f (1 + _) 0