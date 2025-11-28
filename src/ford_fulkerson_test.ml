open Ford_fulkerson
open Graph
open Gfile
open Tools

let test_find_path filename = 
  (* Read graph file. *)
  let graph = from_file ("graphs/" ^ filename ^ ".txt") in 
  (* Find shortest path and make new graph*)
  let graph_short_path = List.fold_left (new_arc) (clone_nodes graph) (find_path graph 0 (n_fold graph (fun x nid -> if x>nid then x else nid) 0)) in
  (* Export graph to Graphvision format. *)
  export ("./graphs/" ^ filename ^ "_shortest_path.gv.txt") graph_short_path ;
  print_endline ("Exported " ^ filename ^ ".txt to Graphviz format after finding shortest path.") ;
  (* Transform graph to SVG. *)
  let ret = Sys.command ("dot -Tsvg ./graphs/" ^ filename ^ "_shortest_path.gv.txt > ./graphs/" ^ filename ^ "_shortest_path.svg") in
  Printf.printf ("Exported shortest path graph to SVG (%d).\n") ret;
  ()

let () = 
  assert (find_path empty_graph 0 1 = []);
  let graph01 = new_node (new_node (new_node empty_graph 0) 1) 2 in
  assert (find_path graph01 0 0 = []);
  assert (find_path (new_arc graph01 {src=0;tgt=1;lbl=0}) 0 1 = [{src=0;tgt=1;lbl=0}]);
  assert (find_path (new_arc graph01 {src=0;tgt=2;lbl=0}) 0 1 = []);

  test_find_path "graph1" ;
  test_find_path "graph2" ;
  test_find_path "graph3" ;
  test_find_path "graph4" ;
  test_find_path "graph5" ;
  test_find_path "graph6" ;
  test_find_path "graph7" ;
  test_find_path "graph8" ;
  test_find_path "graph9" ;
  test_find_path "graph10" ;