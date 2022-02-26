# Vectors

Este es el modulo de vectores de la pregunta 3 del parcial.

## Que provee el modulo?

Adicional a las instrucciones dadas en el parcial (modulo de vectores de 3 dimensiones que sobrecargue los
operadores dados), el modulo funciona para vectores de cualquier dimension codificado a nivel de tipo! , esto significa que por definicion, no se pueden sumar/restar/sacar productos entre dos vectores de distinta dimension, ni tampoco se puede construir un vector cuya longitud no corresponda con la establecida en su tipo (error de compilacion!).

## How to use

El programa debe poseer la siguiente estructura:

```haskell

{-# LANGUAGE DataKinds             #-}
module <nombre del modulo> where

import Prelude hiding (Num(..))
import Vector.VectorSafe

vectorCualquiera :: Vector 5
vectorCualquiera = 1 <:> 20 <:> 26.9 <:> 94 <:> singleton -458.15

v1 :: Vector 3
v1 = 1 <:> 2 <:> singleton 3

v2 :: Vector 3
v2 = 4 <:> -2 <:> singleton 5

suma :: Vector 3
suma = v1 + v2

sumaE :: Vector 2
sumaE = (1 <:> singleton 3) + (2.5 :: Double)

```

Notese que necesitamos la extension `DataKinds` para poder promover los numeros a nivel de tipos (reify?). Adicionalmente, como `Vector` sobrecarga varios operadores de la clase `Num`, escondemos el modulo (y asi nos ahorramos tener que diferenciar entre `(Prelude.+)` y `(VectorSafe.+)` por ejemplo)

Los vectores se construyen a traves de los constructores: `<:>` y `singleton`.

Tratar de sacar producto cruz entre cualquier par de vectores que no posean dimension 3, resultara en un runtime exception.

## Coverage

Para ver el coverage de los test basta con ejecutraR:

```
stack clean
stack test --coverage
```

Sin embargo NO se deje enganar por el bajo `%`, si explora la suite (precisamente `TestVector.TestVector`), vera que la suite de testeo se prueba contra la API (no representacion interna!) porque al fin y al cabo, lo que queremos garantizar es que el cliente obtenga las operaciones que pidio, y que dichas operaciones cumplan los invariantes que debe cumplir.

### Puntos a mejorar de la suite de testeo.

No encontre una forma de generar una instancia de `Arbitrary` para generar vectores de cualquier longitud. Se acepta sabiduria!.


## Fallos/Cosas que no se como arreglar (se acepta iluminacion!).

### No polimorfo :(

Como se puede ver en el ejemplo de `sumaE`, tenemos una anotacion de tipo en `2.5 :: Double`, esto es debido a que no me dio la cabecita para parametrizar
el tipo vector (actualmente es fijo con Double). La razon de esto es porque las instancias de las familias de tipo: 

```haskell
instance Num a => SuperVector (Vec n a) (Vec n a) where
    type V (Vec n a) (Vec n a) = Vec n a
    type S (Vec n a) (Vec n a) = a


instance Num a => SuperVector (Vec n a) a where
    type V (Vec n a) a = (Vec n a)
    type S (Vec n a) a = a
```

Poseen conflictos (el patron `type S (Vec n a) a`, matchea con `type S (Vec n a) (Vec n a)`, pero la variable `a` no es unificable con el tipo `Vec`?).

### Vector de dimension.... 0?

Otra cosa que no me dio la cabecita fue el caso borde:

```haskell
a :: Vector 0
a = undefined
```

Crear este valor esta permitido dentro del sistema de tipos y compila perfectamente, y por ende las siguientes evaluaciones tambien son compilables:

```haskell
a + a
a * a
a - a
a % a
a + 3
a * 3
```

Gracias a dios existen los constraints logicos a nivel de tipo (god bless unification), y esto impide propagar este caso borde, es decir, las siguientes
evaluaciones **NO** validas (soltaran un error de compilacion):

```haskell
a + (1 <:> singleton 2)  -- Longitudes distintas, rechazado
1 <:> a                  -- Falla el constraint de (<:>).
1 <:> 2 <:> a            -- lo mismo, como (<:>) asocia a la derecha, falla en 2 <:> a
```

