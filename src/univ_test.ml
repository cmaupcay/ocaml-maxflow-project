open Ford_fulkerson
open Graph
open Gfile
open Tools

let test_bellman_ford filename = 
  (* Read graph from file and add costs. *)
  let graph = gmap (from_file ("graphs/base/" ^ filename ^ ".txt")) (fun str -> (str, int_of_string str)) in 
  (* Find minimum cost path and make new graph*)
  let graph_short_path = List.fold_left (new_arc) (clone_nodes graph) (bellman_ford graph 0 (n_fold graph (fun x nid -> if x>nid then x else nid) 0)) in
  (* Export graph to Graphvision format. *)
  export ("./graphs/svg/" ^ filename ^ "_min_cost_path.gv.txt") (gmap graph_short_path (fst)) ;
  print_endline ("Exported " ^ filename ^ ".txt to Graphviz format after finding minimum cost  path.") ;
  (* Transform graph to SVG. *)
  let ret = Sys.command ("dot -Tsvg ./graphs/svg/" ^ filename ^ "_min_cost_path.gv.txt > ./graphs/svg/" ^ filename ^ "_min_cost_path.svg") in
  Printf.printf ("Exported min cost path graph to SVG (%d).\n") ret;
  ()

let test_max_flow_min_cost filename = 
  Random.self_init ();
  (* Read graph file. *)
  let graph = from_file ("graphs/base/" ^ filename ^ ".txt") in 
  (* Find max flow with min cost and make new flow graph*)
  let graph_flow = max_flow_min_cost (gmap graph (fun s -> try Scanf.sscanf s "%d %d" (fun flw cst -> (flw, cst))
  with _ -> Printf.printf "Could not read cost and flow from arc in file"; failwith "cots_test"))
    0 (n_fold graph (fun x nid -> if x>nid then x else nid) 0) in
  (* Export graph to Graphvision format. *)
  export ("./graphs/svg/" ^ filename ^ "_flow_cost.gv.txt") (gmap graph_flow (fun (flw, cst) -> String.concat ", " [string_of_int flw; string_of_int cst])) ;
  print_endline ("Exported " ^ filename ^ ".txt to Graphviz format after finding max flow min cost graph.") ;
  (* Transform graph to SVG. *)
  let ret = Sys.command ("dot -Tsvg ./graphs/svg/" ^ filename ^ "_flow_cost.gv.txt > ./graphs/svg/" ^ filename ^ "_flow_cost.svg") in
  Printf.printf ("Exported flow and cost graph to SVG (%d).\n") ret;
  ()

  let test_univ_attribution filename = 
  Random.self_init ();
  (* Read formatted graph file. *)
  let graph = from_file_students ("graphs/student_format/" ^ filename ^ ".txt") in 
  (* Find max flow with min cost and make new flow graph*)
  let graph_flow = max_flow_min_cost graph 0 (n_fold graph (fun x nid -> if x>nid then x else nid) 0) in
  (* Export graph to Graphvision format. *)
  export ("./graphs/svg/" ^ filename ^ "_flow_cost.gv.txt") (gmap graph_flow (fun (flw, cst) -> String.concat ", " [string_of_int flw; string_of_int cst])) ;
  print_endline ("Exported " ^ filename ^ ".txt to Graphviz format after finding max flow min cost graph.") ;
  (* Transform graph to SVG. *)
  let ret = Sys.command ("dot -Tsvg ./graphs/svg/" ^ filename ^ "_flow_cost.gv.txt > ./graphs/svg/" ^ filename ^ "_flow_cost.svg") in
  Printf.printf ("Exported flow and cost graph to SVG (%d).\n") ret;
  let (flow, cost) = get_flow_and_cost graph_flow 0 in
  Printf.printf ("%d students were mapped to universities with total cost %d.\n") flow cost;
  ()

let () = 

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

  test_max_flow_min_cost "students1";
  test_max_flow_min_cost "students2";
  test_max_flow_min_cost "students3";

  assert (get_flow_and_cost graph01 0 = (0,0));
  assert (get_flow_and_cost (new_arc (new_arc graph01 {src=0;tgt=1;lbl=(2,1)}) {src=1;tgt=2;lbl=(2,3)}) 0 = (2,8));

  test_univ_attribution "students_format1";
  test_univ_attribution "students_format2";