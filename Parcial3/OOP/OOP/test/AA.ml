open OUnit2

(*
let aa_tests = "test suite for skew" >::: [
  "skew wiki example" >:: (fun _ -> assert(true));
]
let _ = run_test_tt_main aa_tests;;
*)

open AA


let aa_tests = "test suite for skew" >::: [
  "skew wiki example" >:: (fun _ ->
    let t = Node 
        {   lvl = 1; 
            key = "T"; 
            value="T"; 
            lAA = Node
                {   lvl = 1; key = "L"; value = "L"; 
                    lAA = Node
                        {   lvl = 1; 
                            key = "A"; 
                            value = "A"; 
                            lAA = Empty; 
                            rAA = Empty;
                        };
                    rAA = Node
                        {   lvl = 1; 
                            key = "B"; 
                            value = "B"; 
                            lAA = Empty; 
                            rAA = Empty;
                        };
                }; 
            rAA = Node
                {   lvl = 1; 
                    key = "R"; 
                    value = "R"; 
                    lAA = Empty; 
                    rAA = Empty;
                } 
        
        ;} in
    let t_ = skew(t) in 
    match t_ with 
        | Empty  -> failwith "Empty recibido de un arbol no vacio!"
        | Node {key = "L"; lAA = Node {key="A" ; _}; rAA = Node {value = "T"; lAA = Node {value = "B";_}; rAA = Node {value="R";_}; _}; _} -> assert(true)
        | _ -> failwith "Does not Match!"
    );
]

let _ = run_test_tt_main aa_tests

