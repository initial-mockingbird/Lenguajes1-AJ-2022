type ord = Less | More | Equals ;;


type ('k,'a) aa  
    = Empty  
    | Node of 
        {   lvl   : int; 
            key   : 'k; 
            value : 'a; 
            lAA   : ('k,'a) aa; 
            rAA   : ('k,'a) aa;
        }
;;

let rec toList (t : ('k,'a) aa) : ('k * 'a) list  = 
    match t with 
    | Empty -> []
    | Node {key;value;lAA;rAA;_} ->  (key,value) :: (toList lAA @ toList rAA)
;;

let rec fold (f : 'k -> 'a -> 'b -> 'b) (acc : 'b) (t : ('k,'a) aa) : 'b = 
    match t with 
    | Empty -> acc
    | Node {key;value;lAA;rAA;_} -> f key value @@ fold f (fold f acc lAA) rAA   

let compare (a : 'x) (b : 'x) : ord = 
    if a == b then Equals
    else if a > b 
        then More 
        else Less
;;

let skew (t : ('k,'a) aa) : ('k,'a) aa =
    match t with
    | Empty -> Empty 
    | Node {lAA = Empty ; _} -> t
    | Node ({lvl = t_lvl; lAA = Node ({lvl=l_lvl; rAA = b ; _} as l); _} as t_)-> 
        if t_lvl == l_lvl 
        then Node 
                {l with  
                    rAA = Node {t_ with lAA = b;};
                } 
        else    t
;;


let split (t : ('k,'a) aa) : ('k,'a) aa =
    match t with
    | Empty -> Empty 
    | Node {rAA = Empty ; _} -> t
    | Node {rAA = Node {rAA = Empty; _} ; _} -> t
    | Node ({lvl = t_lvl; lAA=a; rAA = Node ({lAA = b; rAA = Node x ; _} as r) ;  _} as t)->
        if t_lvl <= x.lvl
        then    
                let ret = Node 
                    {r with 
                        lvl = r.lvl + 1;
                        lAA = Node {t with lAA = a; rAA = b;}; 
                        rAA = Node x;
                    } 
                in ret 
        else    Node t
;;


let rec insertWithKey (k : 'k) (a : 'a) (combine : ('k -> 'a -> 'a -> 'a)) (t : ('k,'a) aa) : ('k,'a) aa =
    match t with
    | Empty  -> Node {lvl = 1; key=k; value=a;lAA=Empty;rAA=Empty;}
    | Node t ->  match compare k t.key with 
        | Equals -> Node {t with value = combine k t.value a}            
        | Less   -> Node {t with lAA = insertWithKey k a combine t.lAA } |> skew |> split
        | More   -> Node {t with rAA = insertWithKey k a combine t.rAA } |> skew |> split         
;;

let rec lookup (k : 'k) (t : ('k,'a) aa) : 'a option  =
    match t with 
    | Empty -> None
    | Node t -> match compare k t.key with 
        | Equals -> Some t.value        
        | Less   -> lookup k t.lAA
        | More   -> lookup k t.rAA
;;

let getLevel (t : ('k,'a) aa) : int = 
    match t with
    | Empty -> 0
    | Node {lvl; _} -> lvl
;;

let setLevel (n : int) (t : ('k,'a) aa) : ('k,'a) aa =
    match t with
    | Empty -> Empty
    | Node t_ -> Node {t_ with lvl = n}
;;

let unsafeGetKey (t : ('k,'a) aa) : 'k =
    match t with
    | Node {key ; _} -> key 
    | _          -> assert false
;;

let  decreaseLevel (t : ('k,'a) aa) : ('k,'a) aa =
    match t with
    | Empty -> Empty 
    | Node ({lvl = lvl_t; lAA = l; rAA = r; _} as tr) ->
        let shouldBe = min (getLevel l) (getLevel r) in
        let t1 = Node {tr with lvl = shouldBe} in 
        let t2 = Node {tr with lvl = shouldBe; rAA = setLevel shouldBe r} in 
        if shouldBe < lvl_t 
            then 
                if shouldBe < (getLevel r)
                    then t2
                    else t1
            else t
;;

let  successor (t : ('k,'a) aa) : ('k,'a) aa =
    let rec getMin t = 
        match t with 
        | Empty -> Empty
        | Node ({lAA = Empty ;_} as t_) -> Node {t_ with lvl = 1}
        | Node {lAA = l ; _} -> getMin l 
    in match t with 
    | Empty -> Empty 
    | Node {rAA=Empty ; _} -> Empty
    | Node {rAA=r ; _} -> getMin r
;;

let predecessor  (t : ('k,'a) aa) : ('k,'a) aa =
    let rec getMax t = 
        match t with 
        | Empty -> Empty
        | Node ({rAA = Empty ;_} as t_) -> Node {t_ with lvl = 1}
        | Node {rAA = r ; _} -> getMax r
    in match t with 
    | Empty -> Empty 
    | Node {lAA=Empty ; _} -> Empty
    | Node {lAA=l ; _} -> getMax l
;;

let rec delete (k : 'k) (t : ('k,'a) aa) : ('k,'a) aa =
    let f t = 
        match t with 
        | Node ({rAA = r;_} as t_) -> Node {t_ with rAA = skew r}
        | _ -> t
    in let h t = 
        match t with 
        | Node ({rAA = Node ({rAA=rr ; _} as r_); _} as t_) -> Node {t_ with rAA = Node {r_ with rAA = skew rr}}
        | _ -> t 
    in let g t = 
        match t with 
        | Node ({rAA = r;_} as t_) -> Node {t_ with rAA = split r}
        | _ -> t
    in let newt = match t with 
    | Empty -> Empty
    | Node ({key; rAA = r; lAA = l; _} as t_) ->
        match compare k key with            
        | Less   -> Node {t_ with lAA = delete k l } 
        | More   -> Node {t_ with rAA = delete k r }
        | Equals -> 
            match t with
            | Empty -> assert false
            | Node {lvl=1 ; rAA = r ; _} -> r
            | Node ({lAA = Empty; rAA=r; _} as t_) -> 
                (match successor t with 
                | Empty -> assert false
                | Node {key=k;value=v;_} -> Node {t_ with rAA = delete k r; value=v; key=k})
            | Node ({lAA = l; _} as t_) -> 
                match predecessor t with 
                | Empty -> assert false
                | Node {key=k;value=v;_} -> Node {t_ with lAA = delete k l; value=v; key=k}
    in  decreaseLevel newt 
        |> skew 
        |> f
        |> h 
        |> split 
        |> g  
;;