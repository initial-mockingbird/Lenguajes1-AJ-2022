# Corutinas

Sean las corutinas:


```python

X = 1
Y = 1
Z = 3

A = 2 * (1 + 1 MOD 5) + 3 = 7
B = 2 * (1 + 3 MOD 5) + 3 = 11
C = 2 * (1 + 3 MOD 5) + 3 = 11

coroutine w():
0   int a = 0
    loop:
1       a = a + A
2       print(a)
        if a mod 3 == 0:
4           transfer t()
        else:
5           transfer f()    

coroutine t():
6   int b = 1
    loop:
7       b = (b+1)*B
8       print(b)
        if b mod 3 == 1:
9           transfer w()
        else:
10          transfer f()

coroutine f():
11  int c = 1
    loop:
12      c = 2 * (c + C)
13      print(c)
        if c mod 3 == 2:
14          transfer w()
        else:
15          transfer t()
```

Se puede ver la traza y lo que imprime en el archivo [Corutinas.pdf](Corutinas.pdf)