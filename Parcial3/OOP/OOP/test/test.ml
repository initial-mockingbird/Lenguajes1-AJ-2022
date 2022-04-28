(*open OUnit2
open Set 
*)


(*
let set_gen = QCheck.Gen.(
  (fun self _ -> List.fold_left (fun t x -> t#agregar x; t) (new conjunto) (List.map (fun _ -> Random.int  20) (List.init (Random.int  20) succ))
    ));;



let set_tests = "test suite for OOP" >::: [
  "set example" >:: (fun _ ->
    let s = new conjunto in assert(s#vacio)
    );
]

let _ = run_test_tt_main set_tests

*)

let () = assert(true);;