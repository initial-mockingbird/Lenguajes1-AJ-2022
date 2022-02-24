{-# LANGUAGE DataKinds             #-}
module Main where

import Prelude hiding (Num(..))
import Vector.VectorSafe

main :: IO ()
main = putStrLn "Yup, main is here."

---------------------
-- Algunos ejemplos
---------------------

v1 :: Vector 3
v1 = 1 <:> 10 <:> singleton 20

v2 :: Vector 3 
v2 = 5 <:> -5 <:> singleton 4

suma :: Vector 3 
suma = v1 + v2

resta :: Vector 3 
resta = v1 - v2

sumaE :: Vector 3 
sumaE = v1 + (3 :: Double)

sumaE' :: Vector 3 
sumaE' = (2 :: Double) + v1 

a = (2 :: Double) + v1

mulE :: Vector 3 
mulE = v1 * (2 :: Double)





