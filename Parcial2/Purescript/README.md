# About Purescript....

Es un lenguaje fuertemente tipado, de paradigma (puro) funcional, con (casi) la misma sintaxis de Haskell, pero con pequeñas variaciones que hacen la diferencia:

- Es estricto a diferencia de perezoso.
- Mejora el uso por defecto que tienen los records (al añadir row polimorphism, extensible records, ...)
- Ciertas extensiones de lenguaje de haskell estan activas por defecto en PureScript: `Rank N Types`, `Overloaded Record Syntax` (y con un poco mas azucar permitiendo cosas como: `_.field` para definir funciones accesoras), `DeriveFunctor`...

## Estructuras de Control!

Debido a que tenemos un lenguaje puramente funcional, las estructuras de control comunes tecnicamente hablando dejan de serlo: los `for` solo tienen sentido en ciertos contextos (i.e: `forM`, `sequence`, `traverse`) y claramente son expresiones a diferencia de por ejemplo, los `for` en `C` que son instrucciones del propio lenguaje, lo mismo pasa con los `if`... Y lo mismo pasa con absolutamente TODAS las estructuras de control!

- [Corutinas?](https://github.com/purescript-contrib/purescript-coroutines/tree/main/docs) That's some weird thing based on Category Theory.
- La secuenciacion viene dada por composicion/aplicacion de funciones y sus distintos sabores (composicion normalita, composicion en la categoria de Kleisi, binds,...).
- [Continuations](https://ncatlab.org/nlab/show/continuation+monad) are also a weird Category Theory thing (pero mucho menos que las corutinas! las monads manquesea tienen un sentido algebraico).
- Excepciones! A diferencia de Haskell (las cuales estan definidas como un tipo existencial + un typeclass) estan definidos como imports de funciones foraneas escritas en JS... PERO IGUALMENTE SOLO SE PUEDEN TRABAJAR BAJO `Effect` asi que para efectos practicos, son iwales a las de Haskell.

So yes, we don't need fancy instructions other than pattern matching (which I'm definitely not counting as a `case/switch`).

## Orden de evaluacion!

Purescript utiliza un orden de evaluacion estricto, eso quiere decir que el siguiente programa resultara en un stack overflow:

```haskell
import Data.List 
import Data.List.Types

f :: forall a. a -> List a
f c = Cons c (f c)

strictErr :: List Int 
strictErr = take 3 (f 5)
```

## Tipos!

Y aqui viene lo sabroso, Purescript ofrece los mismos sabores con respecto a tipos que Haskell:

- **Discriminated Unions**: `data DU b a = B b | A a`
- **Anonymous Product**: `data Product a b = P a b`
- **Records**! que a diferencia de Haskell poseen un kind bien especifico: `:kind Record => Row Type -> Type`, algo interesante es que `Row` puede pensarse como un equivalente a `Map Label (NonEmptyList Type)` a nivel de tipos, es decir, no chequea por duplicados! y por lo tanto, el siguiente codigo compila perfectamente:

```haskell
type XxxCollectionxxX = (x :: String, x :: Int, x :: String)

data Ghost r = Boo

x :: Ghost XxxCollectionxxX
x = Boo
```

Una diferencia sutil, es que Purescript trae por defecto: `ExplicitForAll`, por lo cual, para codear una funcion polimorfica, debemos colocar el `forall`:

```haskell
const :: forall a. forall b. a -> b -> a
const x y = x
```

## Sistema de tipos!

Debido a que esta inspirado en haskell, PureScript comparte la mayoria de las caracteristicas con el, incluyendo la equivalencia por nombres: usando `type` creamos un `alias` mientras que usando `newtype` se crea un `alias` incompatible. 

Sin embargo, como PureScript maneja los Records de una manera totalmente distinta, tambien tenemos equivalencia estructural! lo que quiere decir que el siguiente codigo compila:

```haskell
a :: {x :: Int, y :: String}
a = {x:3,y:"a"}

b :: {y :: String, x :: Int}
b = a
```

Pero de lo que si no se salva el lenguaje es que no posee compatibilidad mas alla que la que de la igualdad, nada de casteos automaticos, todos los "casteos" se realizan a mano (entero a flotante, flotante a entero,...).

Y por su poyo tenemos casi el mismo sistema de inferencia de tipos que tenemos en haskell que nos permite hacer type driven developmente:


```haskell
f :: Int -> Int -> Int
f x y = ?g x y 
--      ^^
-- type hole, equivalente al _ de haskell
```

## Como compilar...

El [siguiente link](https://github.com/purescript/documentation/blob/master/guides/Getting-Started.md) tiene una introduccion al lenguaje, sin embargo la version corta es la siguiente:


Primero necesitaremos instalar `spago` (equivaelente a `stack` para haskell):


```bash
npm install -g spago
```

Luego, solo compilamos con:

```bash
$ spago build
```

El archivo [Church.purs](src/Church.purs) contiene la respuesta a la pregunta de los numerales de church, y el archivo
[Tree.purs](src/Tree.purs) al del arbol.