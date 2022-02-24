{-# LANGUAGE DataKinds             #-}
module Main where

import Prelude hiding (Num(..))
import Vector.Vector

main :: IO ()
main = putStrLn "Yup, main is here."

---------------------
-- Algunos ejemplos
---------------------

v1 :: Vector 3 Double
v1 = fromList [1,10,20]

v2 :: Vector 3 Double
v2 = fromList [5,-5,4]

suma :: Vector 3 Double
suma = v1 + v2

resta :: Vector 3 Double
resta = v1 - v2

sumaE :: Vector 3 Double
sumaE = v1 + (5 :: Double)

mulE :: Vector 3 Double
mulE = v1 * (2 :: Double)




