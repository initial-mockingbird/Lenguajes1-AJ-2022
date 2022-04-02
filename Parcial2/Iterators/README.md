# Iterators


## a.)

Enumeramos el programa:

```python
from typing import Any, Iterable, List


 def ins(e : Any, ls : List[Any]) -> Iterable[Any]:
0    yield [e, *ls]
1    if ls:
2        for i in ins(e, ls[1:]):
3            yield [ls[0], *i]

4 for i in ins(0, [1, 2, 3]):
5    print(i)
```

Luego, la corrida esta en: [ins.PDF](ins.PDF)

## b.)

```python
def misterio(ls : List[Any]) -> Iterable[Any]:
0    if ls:
1        for m in misterio(ls[1:]):
2            for i in ins(ls[0], m):
3                yield i
4    else:
5        yield []

6 for m in misterio([1,2,3]):
7    print m
```

### i.)

La corrida esta en: [misterio.PDF](misterio.PDF)

### ii.)

Lo que genera `misterio` son las permutaciones de la lista `ls`, esto es evidente si conocemos lo que hace el iterador `ins`.

`ins(X,xs)` lo que hace es "insertar" `X` en cada posicion de `xs`, es decir, si $xs = [x_0, x_1, \dots x_{n-1}]$ entonces $ys_i = [x_0, \dots , x_{(i-1)}, X, x_i,\dots ,x_{n-1}]$ pertenece a `ins(X,xs)` para todo $0 \leq i \leq n$

Por lo tanto, si pensamos de manera recursiva, en la linea `1` se generan todas as permutaciones de la cola de la lista, y por cada permutacion, insertamos la cabeza en cada posicion posible usando `ins` (linea `2,3`), generando asi todas las permutaciones de la lista actual.

### iii.)

Dos formas de escribir lo que nos piden:

```python
def suspenso(ls : List[Any]) -> Iterable[Any]:
    if ls:
        for m in misterio(ls[1:]):
            for i in ins(ls[0], m):
                for j in i: 
                    yield j

    else:
        yield []

def suspenso1(ls : List[Any]) -> Iterable[Any]:
    for perm in misterio(ls):
        for elem in perm:
            yield elem
```