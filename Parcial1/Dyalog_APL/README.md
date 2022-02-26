# Pregunta 1.

# Que es (Dyalog) APL?

APL es un lenguaje basado en arreglos (precisamente arreglos multidimencionales heterogeneos), debilmente tipeado, y que curiosamente "compila" a bytecode (al menos asi esta implementado en Dyalog), con las decisiones de diseño mas daring que jamas he visto sin ser un lenguaje esoterico:

* Simbolos egipcios? `3≡⌊/×/1+¨ 3 2 ⍴ ⍳6` (y sip, es una expresion valida).
* Operadores sobrecargados.... SOBRE EL NUMERO Y TIPO DE ARGUMENTOS: `/` puede significar tanto repeat: `3/2=2 2 2`, como reduce: `+/1 2 3=6`; `⍳` puede significar: generar un vector a partir de un indice: `⍳ 3 = 1 2 3`, como buscar en que posicion ocurre cada elemento de lo que recibe a su derecha en lo que recibe a su izquierda: `1 2 3 ⍳ 3 5 2 =3 4 2` (suelta 4 porque 5 no pertenece a `1 2 3`).
* Precedencia fija: todos los operadores tienen la misma precedencia: `3 × 5 + 1 = 18`.
* Y mucho mas! 

Como sugiere el titulo, este README trabaja bajo la implementacion/dialecto de Dyalog. Sin embargo, nos limitaremos a tocar solo los elementos que tienen la mayoria de dialectos en comun (asi que nada de OOP o FP bizarro exclusivo de Dyalog).

# Scoping and Binding...

## Alcances: Dinamicos? Si, Estaticos? Tambien. QUE?

APL es un lenguaje bien particular, no solo por los simbolitos raros, falta de precedencia (TODOS los operadores asocian a derecha y tienen la misma precedencia), sino porque tiene DOS formas de definir funciones, con reglas de alcances distintas.

La primera forma es mediante lo que se conoce como `dfns` (direct/defined functions) la cual provee un alcance estatico:

```apl
foo ← 42
]dinput
g ←{
    foo ⍝ Si cambiamos esta y la de f a baz, obtendriamos un runtime exception, otra prueba de que no busca ascendentemente.
}

]dinput
f ← {
    foo ← 24
    g 'dummy arg'
}
⎕ ← f 'dummy arg' ⍝ imprime 42!, puesto que g busca primero en su scope, y luego en global. Nada de buscar ascendentemente.
foo ⍝ imprime 42 tambien, jujuy.
```

Chevere, pero APL tambien provee otra forma de definir funciones, conocida como `tradfns` (traditional functions...) de alcance dinamico. Veamos exactamente el mismo programa pero escrito con la sintaxis de `tradfns`:

```apl
foo ← 42

∇ resG ← g any
    foo
∇

∇ res ← f any
    foo ← 24
    g 'dummy arg'
∇
f 'dummy arg' ⍝ imprime 24!, puesto que g busca primero en su scope, y como no encuentra a foo, busca en el scope de f.
foo ⍝ imprime 24 tambien juju.
```

Finisimo que el lenguaje permita ambas, mas ~~codigo pastichoso~~ libertad. Pero sin duda te preguntaras: como rayos se comportan si se mezclan?

Pues para ti, profesor curioso, te tengo: LA TABLITA:

| Puede el llamado, leer detalles del llamador?      | `dfn`    | `tradfn` |
| ---------------------------------------------------| -------- | -------- |
| `dfn`                                              | prohibio | legal    |
| `tradfn`                                           | legal    | legal    |

Como el ejemplo de arriba sugirio, este tipo de cosas esta PROHIBIDA (runtime exception).

```apl
]dinput
g ←{
    baz ⍝ 
}

]dinput
f ← {
    baz ← 24
    g 'dummy arg'
}
⎕ ← f 'dummy arg' 
```

Sin embargo, estas dos combinaciones son legales:

``` apl
foo ← 42
∇ resG ← g any
    foo
∇

]dinput
f ← {
    foo ← 24
    g 'dummy arg'
}
f 'dummy' ⍝ 24
foo ⍝ 42 still
```

```apl
]dinput
g ←{
    foo 
}
∇ res ← f any
    foo ← 24
    g 'dummy arg'
∇
f 'dummy arg' ⍝ sip, 24 tambien
```


Pero recuerden, no todo lo legal es bueno....

```apl
foo ← 42
]dinput
g ←{
    foo 
}
∇ res ← f any
    foo ← 24
    g 'dummy arg'
∇
f 'dummy arg' ⍝ sip, 24... Yo tambien pensaba que iba a ser 42, la tablita no miente :'v
foo ⍝ positivo, 24
```

Then, why bother? Why not just stick with `dfns`, y hay una lista razonable de razones:

* Losr `dfns` pueden ser anonimas (i.e: sumar 1 al argumento derecho `{⍵+1}`), mientras que los `tradfns` ajuro necesitan un nombre
* Losr `dfns` estan mucho mas cerca de la programacion funcional, pueden referirse a sus argumentos por `⍺,⍺⍺,⍵,⍵⍵` (variable izuierda, operador izquierdo, variable derecha, operador derecho, una mnemotecnia de que alfa ocurre mas a la izquierda que omega en el abecedario), a si mismos (para hacer recursion) mediante: `∇,∇∇`. E implementar pointfree programming bien cheverongo, mientras que los `tradfns` tienen definidos etiquetas para trabajar estructuradamente: `:If, :For, :Goto, :Return`.

## Y las asociaciones?

APL basico posee funciones como second class citizens, y por lo visto en los ejemplos, posee asociaciones profundas al usar `dfns` y asociaciones shallow al usar `tradfns`.


# Modulos! sort of.

Aunque por ningun lado te lo dicen en criollito, APL tiene una _abstraccion_ llamada Workspaces que son equivalentes a los `.m` de Matlab, los `.hs` de Haskell, los `.cpp` de C++.... Estos se crean mediante:

```
<secuencia de comandos que crean funciones y variables>

)save <aqui va el path entero de a donde lo quieres guardar>
```

Asi mismo, para cargar un archivo:

```
)load <el path entero>
```

Adicionalmente, si no se desea modificar el workspace, sino solo importar sus definiciones, podemos usar el comando: `)copy`.

Para imprimirlas variables de nuestro workspace, basta con ejecutar `)vars`, y para ver las funciones: `)fns`.

Las variables pueden ser borradas mediante `)erase`, y podemos volver al workspace inicial (sin ninguna definicion) mediante `)clear`.

Digo _abstraccion_, porque APL **no** posee una extension de archivo asociado, sino que varia con el dialecto que se use, mas aun, el archivo NO es guardado en texto plano (por lo general), sino en una especie de binario, so yes, there is that.

Pero estamos aca para hablar de modulos, y tecnicamente hablando, si, APL posee modulos, estos son llamados `namespaces` y pueden contener una coleccion de workspaces u otros namespaces.

Hay varias maneras de crear un namespace, una en particular es mediante: `)NS <nombre de tu namespace>`.

Adicionalmente se puede obtener un listado de todos los namespaces creados: `)Obs`.

Finalmente, podemos copiar declaraciones dentro de nuestro namespace mediante: `<nombre del namespace> ← ⎕NS <lista de cosas que meter>`. Todo esto lo podemos ver en el siguiente mini-ejemplo:

```apl
        )NS Parcialito
#.Parcialito
        )Obs
Parcialito      
        Parcialito
#.Parcialito
        datos1 ← 1 2 3 4 5
        add1   ← {⍵ + 1}
        add5   ← {⍵ + 5}
        map5   ← {add5¨⍵}

        Parcialito ← ⎕NS 'datos1' 'add1' 'add5' 'map5'
```

Y ahora podemos volver a nuestro namespace original y utilizar las cosas de Parcialito:

```apl
        )CS ⍝Volver al namespace raiz.
#
        )Obs ⍝ Aqui ta parcialito
Parcialito      
        Parcialito.datos1 ⍝ sip, fully qualified names outside.
1 2 3 4 5

        )save <path pa wardar, tan solo crear el objeto no es suficiente :'v>
```

(Nota del estudiante: es TERRIBLE trabajar en el "repl" de Dyalog, asi que en vez de trabajar pelao, recomiendo fuertemente tener otro editor, o usar el comando `)ed` para abrir un editor en dyalog con... actual lines. Si se quiere crear un namespace entonces: `)ed ⍟ <nombre>` creara un template pal namespace.)

Para APL, los namespaces poseen una estructura de tipo directorio, asi que si declarasemos un modulo dentro de parcialito, pudiesemos entrar a el, y modificarlo, modificando asi a parcialito, ademas tambien provee un sin fin de utilidades adicionales que estan completamente fuera del scope de la pregunta.

Ahora, consideraria que estos modulos son abiertos o cerrados? Segun lo que hemos visto son selectivamente abiertos, pues siempre puedes importar/exportar cosas de manera selectiva gracias a que el objeto que guarda el namespace es en realidad una referencia (mas en la siguiente seccion!).

# Aliases, Sobrecarga y Polimorfismo.

Afrontemolo, vanilla APL solo posee 3 tipos de datos: numeros, strings y Arreglos "heterogeneos" (pueden almacenar numeros, strings u otros arreglos a la vez).Asi que el polimorfismo esta fuera de ranking aca.

Y aunque tecnicamente si tiene sobrecarga de operadores (vease el `/` o el `⍳` en la descripcion), no es posible sobrecargar los operadores as a user.

Sin embargo, algo que si tiene APL son aliases (bien malandros tambien). Siguiendo la linea de modulos:


```apl
    P ← Parcialito

    Parcialito.datos1
1 2 3 4 5

    P.datos1
1 2 3 4 5

    P.datos1 +← 1

    P.datos1
2 3 4 5 6

      Parcialito.datos1
2 3 4 5 6
```
So yes, la asignacion `P ← Parcialito` efectivamente copia el contenido de `Parcialito`, sin embargo `Parcialito` en realidad es una referencia, por lo tanto `P` copia esa referencia, no el namespace.

Algo medio importante que resaltar es que esto no hubiese pasado si se trabajase con el arreglo normalito:

```apl
    p ← 1 2 3
    q ← p
    p
1 2 3

    q
1 2 3

    p+←1
    p
2 3 4

    q
1 2 3

    q+←5
    p
2 3 4

    q
6 7 8
```

# Toolings!

Bueno, el dialecto con el que trabajo en realidad es propietariado (aun gratis mientras no se use para ganar dinerito), y trae por defecto un IDE, Debugger, varios namespaces para trabajar con el GUI (i.e:crear botones y asi) y una extension a APL que le permite trabajar OOP y FP (con clases, herencia, metodos publicos y privados...).

# Maxima potencia.

Una forma bien sencilla de definir la potencia en APL es:

```apl
  ⍝ esto es un comentario
  pow ← {×/⍵/⍺}
  3 pow 2
  9
  ⍝ 3 pow 2 
  ⍝ 3 {×/⍵/⍺} 2
  ⍝ ×/3/2
  ⍝ ×/(3/2)
  ⍝ ×/ 3 3
  ⍝ 3 × 3
  ⍝ 9
```

Tambien lo podemos hacer recursivo:

```apl
  ⍝ it's all coming together.
  ]dinput
  powRec ← {
      ⍵ ≡ 0 : 1   ⍝ si el arg derecho es 0, retorna 1
      ⍺ × ⍺ ∇ ⍵-1 ⍝ sino, recursionnn 
                  ⍝ ∇ es una forma de llamar a la 
                  ⍝funcion, creo que tambien se puede ⍝ usar powRec, pero ∇ es mas rapido).
  } 

  3 powRec 2
9
```
Algo que quizas no he mencionado, es que las funciones en APL retornan el primer valor que se evalua, por eso despues de la guardia: `⍵ ≡ 0 :` colocamos un `1`. Si hubiesemos colocado un `algo ← 1`, hubiese pasado a la siguiente linea y tendriamos recursion infinita.

# MATriarchy

APL es un lenguaje basado en arreglos, por lo tanto, esto es literal una linea:

```apl
    ⍝ el . es el producto interno de arreglos(ish)
    
    mproduct ← {⍺+.×⍵}
    M ← 3
    N ← 2
    P ← 3
    ⍝ M N ⍴ 1 2 3 4 5 6
    ⍝ 3 2 ⍴ 1 2 3 4 5 6
    ⍝ el operador ⍴ reshapes el vector 1 2 3 4 5 6
    ⍝ y lo fuerza a tener dimension 3x2.
    mat1 ← M N ⍴ 1 2 3 4 5 6
    mat1
1 2
3 4
5 6

    mat2 ← N P ⍴ 7 8 9 10 11 12
    mat2
 7  8  9
10 11 12

    mat1 mproduct mat2
27  30  33
61  68  75
95 106 117

    ⍝ mat1 {⍺+.×⍵} mat2
    ⍝ 1 2          7  8  9
    ⍝ 3 4 {⍺+.×⍵} 10 11 12
    ⍝ 5 6     
    ⍝ +/(1*7 2*10)  +/(1*8 2*11) +/(1*9 2*12)
    ⍝    :               :           :
    ⍝    :               :           :
    ⍝ Basicamente . para matrices/arrelgosish es equivalente a un:
    ⍝ (innerProd) :: (a->b->c) ->(c->c->c) -> Mat a -> Mat b -> Mat c
    ⍝ innerprod combine reduce ⍺ ⍵ = foldl1' reduce $ zipWith combine ⍺ ⍵  

```

# Gracias :D Espero le haya gustado.