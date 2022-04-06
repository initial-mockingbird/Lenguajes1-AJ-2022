module Tree where

import Data.Maybe (Maybe(..), isJust)
import Data.Tuple (Tuple(..))
import Prelude

import Control.Apply (lift2)
import Data.Ord (abs)

data Arbol a = Hoja a | Rama a (Arbol a) (Arbol a)

foldTree :: forall a. forall b. (a -> b -> b -> b) -> (a -> b) -> Arbol a -> b
foldTree  _ g (Hoja x)     = g x
foldTree  f g (Rama x l r) = f x (h l) (h r) 
    where
        h = foldTree f g

esMinHeapBalanceado :: forall a. Ord a => Arbol a -> Boolean
esMinHeapBalanceado = isJust <<< foldTree f g
    where
        g x = Just $ Tuple x 1
        f val =  (<<<) join <<< lift2 (h val)

        -- un minheap esta balanceado si...
        h :: forall a. Ord a => a -> Tuple a Int -> Tuple a Int -> Maybe (Tuple a Int)
        h x (Tuple l dl) (Tuple r dr) 
            -- cada nodo es menor que su hijo izquierdo y derecho y...
            -- la diferencia entre las alturas de izquierda y derecha es 1....
            | x <= l && x <= r && abs (dl-dr) == 1 = Just $ Tuple x $ max dl dr + 1
            | otherwise = Nothing
