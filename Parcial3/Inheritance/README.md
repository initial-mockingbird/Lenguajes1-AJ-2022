# Inheritance

There are no tests!

BUT tiene una extension pa compensar eso :'v

Con la sintaxis: `CLASS <tipo> : {[tipo]} [nombre]` puedes expresar herencia multiple como
en el ejemplo:

```java
>>> CLASS A f g         
>>> CLASS B : A f h
>>> CLASS C : {A B} j k
>>> DESCRIBIR A
f -> A :: f
g -> A :: g
>>> DESCRIBIR B
f -> B :: f
g -> A :: g
h -> B :: h
>>> DESCRIBIR C
f -> B :: f
g -> A :: g
h -> B :: h
j -> C :: j
k -> C :: k
```

