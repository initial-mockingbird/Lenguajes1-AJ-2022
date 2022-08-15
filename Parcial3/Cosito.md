# Pregunta 1

[Aqui en Github](https://github.com/initial-mockingbird/Lenguajes1-AJ-2022/tree/main/Parcial3/OOP/OOP)

# Pregunta 2

[Aqui en github](https://github.com/initial-mockingbird/Lenguajes1-AJ-2022/tree/main/Parcial3/concurrent)

# Pregunta 4

[Aqui en github](https://github.com/initial-mockingbird/Lenguajes1-AJ-2022/tree/main/Parcial3/Inheritance)

# Pregunta 5.b

Esta ya la tenia implementada en calculadora del parcial 2 de manera mas general:

```haskell
data BinTree a b = Leaf b | Node a (BinTree a b) (BinTree a b) deriving (Show, Eq)

foldTree :: (a -> b -> b -> b) -> (c -> b) -> BinTree a c -> b 
foldTree _ g (Leaf x) = g x
foldTree f x (Node a l r) =  f a lVal rVal
    where 
        lVal = foldTree f x l
        rVal = foldTree f x r
```

Para `foldA` sera:

```haskell
data Arbol a = Leaf a | Node a (Arbol a) (Arbol a ) deriving (Show, Eq)

foldA :: (a -> b -> b -> b) -> b -> Arbol a -> b 
foldA _ g (Leaf _) = g 
foldA f x (Node a l r) =  f a lVal rVal
    where 
        lVal = foldA f x l
        rVal = foldA f x r
```

