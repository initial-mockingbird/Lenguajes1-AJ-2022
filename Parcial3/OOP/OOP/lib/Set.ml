open AA;;

exception NonExistingKeyError of string

type 'k conjunto = ('k,unit) aa  
type 'k bolsa    = ('k,'k list) aa

module type Foldable_Core =
    sig 
        type 'a t
        val foldl : ('a -> 'b -> 'b) -> 'b -> 'a t -> 'b 
    end 
;;

module Foldable_Utils(Core : Foldable_Core) =
    struct 
        include Core
        let toList (ts : 'a Core.t) = Core.foldl List.cons [] ts
        let length (ts : 'a Core.t) = Core.foldl (fun _ n -> n + 1) 1 ts
    end
;;


module type Coleccion  =
    sig 
        type 'x t
        val empty   :  'x t 
        val agregar : 'k -> 'k t -> 'k t
        val remover : 'k -> 'k t -> 'k t
        val vacio   : 'k t -> bool
        val member  : 'k -> 'k t -> bool
    end
;;


module Conjunto : Coleccion with type 'x t = 'x conjunto =
    struct
        type 'x t     = 'x conjunto
        let empty     = Empty
        let agregar x repr = insertWithKey x () (fun _ _ a -> a) repr
        let member x  repr = lookup x repr <> None
        let remover x repr = let t = lookup x repr in 
            match t with
            | None  -> raise (NonExistingKeyError "El elemento no pertenece al conjunto!")
            | _     -> delete x repr
        let vacio  repr   = repr == Empty
    end
;;

module Foldable_Set_Core : Foldable_Core with type 'x t = 'x conjunto = 
    struct
        type 'x t     = 'x conjunto
        let foldl f acc ts = fold (fun k _ b -> f k b) acc ts
    end
;;


module Foldable_Set = Foldable_Utils(Foldable_Set_Core) ;;

module Bolsa : Coleccion =
    struct
        type 'x t = 'x bolsa 
        let empty   = Empty
        let agregar x repr =   insertWithKey x [x] (fun _ acc a -> a @ acc) repr
        let member  x repr = lookup x repr<> None
        let remover x repr = let t = lookup x repr in 
            match t with
            | None  -> raise (NonExistingKeyError "El elemento no pertenece al conjunto!")
            | Some [_] ->   delete x repr
            | _        ->   insertWithKey x [] (fun _ xs _ -> List.tl xs) repr
        let vacio     repr = repr == Empty
    end 
;;

