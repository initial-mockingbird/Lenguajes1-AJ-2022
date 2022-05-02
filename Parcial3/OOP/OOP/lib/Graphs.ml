


open AA;;
open Set;;


class virtual graph =
    object (_ : 'self)
        method virtual add:     int -> 'self
        method virtual successors: int -> int Conjunto.t
    end 
;;

class virtual busqueda =
    object
        method virtual buscar: int -> int -> int
    end
;;



class adj_graph =
    object
        inherit graph 
        val _adj_list : (int,int Conjunto.t) aa = Empty

        method successors (v : int) : int Conjunto.t = 
            match lookup v _adj_list with
            | None    -> Conjunto.empty
            | Some vs -> vs 
        method add (v : int) =  {< _adj_list = insertWithKey v Conjunto.empty (fun k xs _ -> Conjunto.agregar k xs ) _adj_list >}

    end
;;


class dfs (g : graph) =
    object 
        inherit busqueda
        method buscar (s : int) (t : int) : int = 
            let rec f (_lifo : int list) (mem : int Conjunto.t) : int  = 
            match _lifo with
            | []      -> -1 
            | (x::xs) -> if x == t 
                then 0 
                else    let succList = Foldable_Set.toList @@ g#successors x in 
                        let news     = List.filter (fun x -> not @@ Conjunto.member x mem ) succList in
                        let newMem   = List.fold_left (fun oldMem elem -> Conjunto.agregar elem oldMem) mem news in 

                (fun x -> 1 + x) @@ f (news @ xs) newMem
            in f [s] Conjunto.empty
    end
;;


class bfs (g : graph) =
    object 
        inherit busqueda
        method buscar (s : int) (t : int) : int = 
            let rec f (current : int Conjunto.t) (mem : int Conjunto.t) : int  = 
            match Conjunto.vacio current with
            | true      -> -1 
            | _  -> if Conjunto.member t current 
                then 0 
                else    let setUnion s1 s2 = Foldable_Set.foldl Conjunto.agregar s2 s1  in 
                        let g x acc  = if not @@ Conjunto.member x mem then g#successors x |> setUnion acc else acc in
                        let news     = Foldable_Set.foldl g Conjunto.empty current in 
                        let newMem   = setUnion mem news in

                (fun x -> Foldable_Set.length current + x) @@ f news newMem
            in f (Conjunto.agregar s Conjunto.empty) Conjunto.empty
    end
;;
