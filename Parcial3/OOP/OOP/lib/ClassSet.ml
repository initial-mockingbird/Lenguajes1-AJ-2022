open AA;;

exception NonExistingKeyError of string

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

class ['k] bolsa =
    object 
        inherit ['k] coleccion 
        val _d   = Empty
        method agregar x = {< _d = insertWithKey x [x] (fun _ acc a -> a @ acc) _d >}
        method member  x = lookup x _d <> None
        method remover x = let t = lookup x _d in 
            match t with
            | None  -> raise (NonExistingKeyError "El elemento no pertenece al conjunto!")
            | Some [_] -> {< _d = delete x _d >}
            | _        -> {< _d = insertWithKey x [] (fun _ xs _ -> List.tl xs) _d >}
        method vacio     = _d == Empty
        method toList    = List.concat @@ List.map (fun pair -> snd pair) @@ toList _d
    end
;;