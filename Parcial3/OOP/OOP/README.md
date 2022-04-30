# oCAML

oCAML es un lenguaje funcional.
oCAML es un lenguaje orientado a objetos.
oCAML es un lenguaje imperativo.
oCAML es un lenguaje modular.

Quien haya dicho: Jack of all trades is master of None claramente no conocia a oCAML.

## Objetos: de todas las formas y sabores!

Formas de declarar y crear objetos tenemos muchas, declaracion:

```ocaml
class ['k] conjunto =
    object
        inherit ['k] coleccion 
        val  _d : ('k,unit) aa = Empty
        method agregar x = {< _d = insertWithKey x () (fun _ _ a -> a) _d >}
        method member x  = lookup x _d <> None
        method remover x = let t = lookup x _d in 
            match t with
            | None  -> raise (NonExistingKeyError "El elemento no pertenece al conjunto!")
            | _     -> {< _d = delete x _d >}
        method vacio     = _d == Empty
        method private toList    = List.map (fun pair -> fst pair) @@ toList _d
    end
;;
```

Define una clase `conjunto`, con variable de tipo `'k`, la cual hereda de la clase `coleccion`, tiene como miembro `_d` (no mutable!), y como metodos de la clase `agregar,member,remover,vacio,toList`, con `toList` privado.

Si observamos el metodo agregar:

```ocaml
method agregar x = {< _d = insertWithKey x () (fun _ _ a -> a) _d >}
```

Notamos que tiene una notacion bien particular, en general, cualquier metodo que este definido como: 
`{<variable = nuevo Valor>}` va a retornar una copia de la instancia del objeto con los valores actualizados, siguiendo la filosofia de haskell con sus record updates.

Si al cambio, necesitaramos mutar un valor, tendriamos que redefinir la clase de la siguiente manera:

```ocaml
class ['k] conjunto =
    object
        inherit ['k] coleccion 
        val mutable  _d : ('k,unit) aa = Empty
        method agregar x = _d <- insertWithKey x () (fun _ _ a -> a) _d 
    end
;;
```

En este caso, ya agregar no devolveria ningun valor, sino que devolveria unidad, representando que hubo un cambio en el objeto.

Adicionalmente, pudiesemos tambien crear objetos sin la necesidad de asociarlos a una clase:


```ocaml
let conjunto = 
    object 
        val mutable  _d : ('k,unit) aa = Empty
        method agregar x = _d <- insertWithKey x () (fun _ _ a -> a) _d 
    end 
in conjunto#agregar 5; conjunto#_d
;; 
```

El cual definiria un objeto con atributo `_d` y metodo `agregar`, luego agregaria `5`, y finalmente retornaria el conjunto que posee dentro.

Pero mas interesante aun, es que en oCAML no se suelen utilizar clases (a menos que de verdad necesites la herencia), esto es debido a que hay una generalizacion mucho mas de pinga: MODULOS!


Sip, los modulos aqui tienen un poder bastante similar al de las clases, puesto que estan dotados de una abstraccion llamada Functores, la cual se puede ver como un mapeo de modulos a modulos.

Esto se puede ejemplificar mejor con la siguiente situacion totalmente ficticia...

### La travesia del alumno

Supongamos que a un alumno le mandaron a desarrolar una libreria para manejar colecciones: conjuntos, y el alumno propone la siguientes clases para cada quien:

```ocaml
class virtual ['k] coleccion =
    object (_ : 'self)
        method virtual agregar: 'k -> 'self
        method virtual remover: 'k -> 'self
        method virtual vacio  : bool
        method virtual member : 'k -> bool
        method virtual toList : 'k list
    end
;;



class ['k] conjunto =
    object
        inherit ['k] coleccion 
        val  _d : ('k,unit) aa = Empty
        method agregar x = {< _d = insertWithKey x () (fun _ _ a -> a) _d >}
        method member x  = lookup x _d <> None
        method remover x = let t = lookup x _d in 
            match t with
            | None  -> raise (NonExistingKeyError "El elemento no pertenece al conjunto!")
            | _     -> {< _d = delete x _d >}
        method vacio     = _d == Empty
        method toList    = List.map (fun pair -> fst pair) @@ toList _d
    end
;;
```

Muy genial, pero luego el alumno se da cuenta que tanto member, vacio y remover son un caso especifico del typeclas foldable que vio en el laboratorio de lenguajes, asi que decide crear una clase que haga el paro:

```ocaml

class virtual ['a,'t] foldable =
    object
        fold : 'b. ('a -> 'b -> 'b) -> 'b -> 'a 't -> 'b
        toList = fold cons [] 
    end
;;
```

Necesitamos introducir el `'b.` al inicio de la firma, puesto que de otra forma, oCAML se quejaria por unbounded types, y nos forzaria a anadirla en el tope de la clase, algo malo puesto que `'b` no determina en NINGUNA manera
la clase `foldable`.

Sin embargo, al tratar de compilar esto, ocaml se queja, puesto que `'t` solo puede ser un "ground term", es decir, algo cuyo kind es `*`, no `* -> *`. Por lo tanto necesitamos una nueva abstraccion que nos permita manejar esto:

```ocaml

module type Foldable_Core =
    sig 
        type 'a t
        val fold : ('a -> 'b -> 'b) -> 'b -> 'a t -> 'b 
    end 
;;
```

Y aqui vienen los modulos!, como dentro de un modulo, puede ir lo que sea pueda ir en un programa, vamos a declarar el modulo de tipo `Foldable_Core` que albergue de manera estatica el tipo `t` adicional a nuestra funcion `fold`. Ahora si quisieramos derivar una "instancia" solo tenemos que:


```ocaml
module Foldable_List_Core : Foldable_Core with type 'a t = 'a list =
    struct
        type 'a t = 'a list
        let rec fold = List.fold_left
    end
;;
```

Notemos que podemos hacer esto para cualquier tipo, lo que lo hace bastante similar a las familias de tipos en haskell con un pequeño caveat: dado que oCAML diferencia los tipos por nombres y no contenido, necesitamos indicar explicitamente que el tipo `'a t` unifica con el tipo `'a list`, o de otra forma nunca podriamos sacar los valores fuera del modulo.

Sin embargo, en esta implementacion, falta la verdadera salsa, los comportamientos por defecto!

Como en los `module type` solo pueden ir declaraciones de tipo, necesitamos crear una instancia que: dada una instancia de `Foldable_Core` la decore con utilidad adicional:


```ocaml
module Foldable_Utils(Core : Foldable_Core) =
    struct 
        include Core
        let toList (ts : 'a Core.t) = Core.foldl List.cons [] ts
        let length (ts : 'a Core.t) = Core.foldl (fun _ n -> n + 1) 1 ts
    end
;;
```

Notemos que este modulo: exporta todo lo que trae el modulo `Core` mediante `include`, y define dos funciones por defecto. Una vez hecho esto, ya podemos crear instancias con utilidades:



```ocaml
module Foldable_List = Foldable_Utils(Foldable_List_Core);;
```

Tal cual como una funcion!

## Memoria

Sip, garbage collection, oCAML tiene un GRAN enfoque en programacion funcional, hasta tal punto que parece querer deshacerse de las clases (why use clases when you have modules?)

Pero eso era de esperarse, lo verdaderamente interesante es: que tecnicas de garbage colelction utiliza?

Pues oCAML implemente un hibrido entre recoleccionde basura generacional e incremental, lo que significa que tiene dos heaps: uno pequeno para los nuevos objetos, y uno mas grande para los objetos viejosm claramente es incremental, puesto que el recolectorsolo se llama de manera recurrente en el minor heap.

## Asociacion estatica o dinamica?

Definitivamente dinamica, la magia con los modulos solo se puede realizar cuando tienes asociacion dinamica, mas aun, dado que las clases son "wrappers" para objetos inmediatos, y estos se pueden llamar ad-hoc, no tiene mucho sentido tener asociacion estatica de metodos (o mejor dicho, seria una tarea descolosal).

## Tipos <3

Empezando con los mecanismos de herencia multiple, en oCAML they are **very** discouraged, but if you really like'em then....

```ocaml
class gg =
    object
        val nombre = "Joseph"
        val stando = "Hermito Purple"
        method getNombre = nombre
        method private getStando = stando
        method x = "y"
    end
;;

class haha = 
    object
        val nombre = "sushi Q"
        method getNombre = nombre
        method x         = "x"
    end
;;

class jojo =
    object
        inherit gg as gg
        inherit haha as haha
        val nombre = "Jotaro"
        val stando = "STAR PLATINUM"
        method getNombre = nombre
        method getStando = 
            let () = print_string (gg#getStando) in 
            let () = print_string "\n" in 
            let () = print_string (gg#getNombre) in
            let () = print_string "\n" in 
            let () = print_string (haha#getNombre) in 
            let () = print_string "\n" in 
            stando
    end
;;

let j = new jojo;;
j#getStando;;
STAR PLATINUM <- atributo sobreescrito
Jotaro        <- atributo sobreescrito
Jotaro        <- atributo sobreescrito
- : string = "STAR PLATINUM"

j#x;;
- : string = "x" <- haha es declarada de ultimo, por lo tanto sobreescribe a gg.
```

Dado que oCAML posee despacho dinamico, todo se comporta como uno espera: lo ultimo ejecutado es lo que se mantiene.

Sobre el polimorfismo parametrico, ya lo hemos hablado en la seccion anterior cuando tocamos las clases, podemos manejar el polimorfismo anadiendo explicitamente variables de tipo en las clases, pero en caso de que tengamos un metodo que realmente sirva para cualquier tipo, lo podemos expresar como si fuese un para todo explicito.

Sobre la varianza... oh god.

Es bien chevere, oCAML te da el poder para expresar explicitamente la varianza de un tipo:

```ocaml
type + 'a T <- T a es un tipo covariante
type - 'a T <- T a es un tipo contravariante
```

Sin embargo, el programador decide no anotar (posiblemente todas) las varianzas, oCAML aun tiene la posibilidad de inferirlas como infiera los tipos... Just absolute magic.

(No type hierarchy porque no entendi bien que se preguntaba :'v).

## CONSIDERACIONES

La implementacion de conjuntos mediante clases se encuentra en [./lib/ClassSet.ml](./lib/ClassSet.ml), pero una mas chevere con modulos en [./lib/Set.ml](./lib/Set.ml), ambas usan una diccionario implementado como arbol AA de fondo.

Los grafos estam en [./lib/Graphs.ml](./lib/Graphs.ml).
