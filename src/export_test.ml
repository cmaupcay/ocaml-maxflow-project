open Gfile

(* Text export function with a textfile representing a graph. *)
let test_export filename = 
  (* Read graph file. *)
  let graph = from_file ("graphs/" ^ filename ^ ".txt") in 
  (* Export graph to Graphvision format. *)
  export ("./graphs/" ^ filename ^ ".gv.txt") graph ;
  print_endline ("Exported " ^ filename ^ ".txt to Graphviz format.") ;
  (* Transform graph to SVG. *)
  let ret = Sys.command ("dot -Tsvg ./graphs/" ^ filename ^ ".gv.txt > ./graphs/" ^ filename ^ ".svg") in
  Printf.printf ("Exported graph to SVG (%d).\n") ret;
  ()

let () =
  test_export "graph1" ;
  test_export "graph2" ;
  test_export "graph3" ;
  test_export "graph4" ;
  test_export "graph5" ;
  test_export "graph6" ;
  test_export "graph7" ;
  test_export "graph8" ;
  test_export "graph9" ;
  test_export "graph10" ;

  test_export "graph1_cost" ;
  test_export "graph2_cost" ;
  test_export "graph3_cost" ;
  test_export "graph4_cost" ;
  test_export "graph5_cost" ;
  test_export "graph6_cost" ;
  test_export "graph7_cost" ;
  test_export "graph8_cost" ;
  test_export "graph9_cost" ;
  test_export "graph10_cost" ;

  test_export "students1" ;
  test_export "students2" ;