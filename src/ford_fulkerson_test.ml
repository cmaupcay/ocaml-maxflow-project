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

let test_max_flow filename = 
  (* Read graph file. *)
  let graph = from_file ("graphs/" ^ filename ^ ".txt") in 
  (* Find max flow and make new flow graph*)
  let graph_flow = max_flow (gmap graph int_of_string) 0 (n_fold graph (fun x nid -> if x>nid then x else nid) 0) in
  (* Export graph to Graphvision format. *)
  export ("./graphs/" ^ filename ^ "_flow.gv.txt") (gmap graph_flow string_of_int) ;
  print_endline ("Exported " ^ filename ^ ".txt to Graphviz format after finding max flow graph.") ;
  (* Transform graph to SVG. *)
  let ret = Sys.command ("dot -Tsvg ./graphs/" ^ filename ^ "_flow.gv.txt > ./graphs/" ^ filename ^ "_flow.svg") in
  Printf.printf ("Exported flow graph to SVG (%d).\n") ret;
  ()

let test_bellman_ford filename = 
  (* Read graph from file and add costs. *)
  let graph = gmap (from_file ("graphs/" ^ filename ^ ".txt")) (fun str -> (str, int_of_string str)) in 
  (* Find minimum cost path and make new graph*)
  let graph_short_path = List.fold_left (new_arc) (clone_nodes graph) (bellman_ford graph 0 (n_fold graph (fun x nid -> if x>nid then x else nid) 0)) in
  (* Export graph to Graphvision format. *)
  export ("./graphs/" ^ filename ^ "_min_cost_path.gv.txt") (gmap graph_short_path (fst)) ;
  print_endline ("Exported " ^ filename ^ ".txt to Graphviz format after finding minimum cost  path.") ;
  (* Transform graph to SVG. *)
  let ret = Sys.command ("dot -Tsvg ./graphs/" ^ filename ^ "_min_cost_path.gv.txt > ./graphs/" ^ filename ^ "_min_cost_path.svg") in
  Printf.printf ("Exported min cost path graph to SVG (%d).\n") ret;
  ()

let test_max_flow_min_cost filename = 
  (* Read graph file. *)
  let graph = from_file ("graphs/" ^ filename ^ ".txt") in 
  (* Find max flow with min cost and make new flow graph*)
  let graph_flow = max_flow_min_cost (gmap graph (fun s -> try Scanf.sscanf s "%d %d" (fun flw cst -> (flw, cst))
  with _ -> Printf.printf("Could not read cost and flow from arc in file")))
    0 (n_fold graph (fun x nid -> if x>nid then x else nid) 0) in
  (* Export graph to Graphvision format. *)
  export ("./graphs/" ^ filename ^ "_flow_cost.gv.txt") (gmap graph_flow (fun (flw, cst) -> String.concat ", " [string_of_int flw, string_of_int cst])) ;
  print_endline ("Exported " ^ filename ^ ".txt to Graphviz format after finding max flow graph.") ;
  (* Transform graph to SVG. *)
  let ret = Sys.command ("dot -Tsvg ./graphs/" ^ filename ^ "_flow_cost.gv.txt > ./graphs/" ^ filename ^ "_flow_cost.svg") in
  Printf.printf ("Exported flow and cost graph to SVG (%d).\n") ret;
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

  assert (max_flow empty_graph 0 1 = empty_graph);
  let graph01 = new_node (new_node (new_node empty_graph 0) 1) 2 in
  assert (max_flow graph01 0 0 = clone_nodes graph01);

  test_max_flow "graph1" ;
  test_max_flow "graph2" ;
  test_max_flow "graph3" ;
  test_max_flow "graph4" ;
  test_max_flow "graph5" ;
  test_max_flow "graph6" ;
  test_max_flow "graph7" ;
  test_max_flow "graph8" ;
  test_max_flow "graph9" ;
  test_max_flow "graph10" ;
  test_max_flow "graph_cycle" ;

  assert (bellman_ford empty_graph 0 1 = []);
  let graph01 = new_node (new_node (new_node empty_graph 0) 1) 2 in
  assert (bellman_ford graph01 0 0 = []);
  assert (bellman_ford (new_arc graph01 {src=0;tgt=1;lbl=(0,1)}) 0 1 = [{src=0;tgt=1;lbl=(0,1)}]);
  assert (bellman_ford (new_arc graph01 {src=0;tgt=2;lbl=(0,1)}) 0 1 = []);

  test_bellman_ford "graph1" ;
  test_bellman_ford "graph2" ;
  test_bellman_ford "graph3" ;
  test_bellman_ford "graph4" ;
  test_bellman_ford "graph5" ;
  test_bellman_ford "graph6" ;
  test_bellman_ford "graph7" ;
  test_bellman_ford "graph8" ;
  test_bellman_ford "graph9" ;
  test_bellman_ford "graph10" ;

  test_max_flow_min_cost "graph1_cost" ;
  test_max_flow_min_cost "graph2_cost" ;
  test_max_flow_min_cost "graph3_cost" ;
  test_max_flow_min_cost "graph4_cost" ;
  test_max_flow_min_cost "graph5_cost" ;
  test_max_flow_min_cost "graph6_cost" ;
  test_max_flow_min_cost "graph7_cost" ;
  test_max_flow_min_cost "graph8_cost" ;
  test_max_flow_min_cost "graph9_cost" ;
  test_max_flow_min_cost "graph10_cost" ;