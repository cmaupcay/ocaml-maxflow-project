open Ford_fulkerson
open Graph
open Gfile
open Tools


  let univ_attribution input output  = 
  Random.self_init ();
  (* Read formatted graph file. *)
  let graph = from_file_students (input) in 
  (* Find max flow with min cost and make new flow graph*)
  let graph_flow = max_flow_min_cost graph 0 (n_fold graph (fun x nid -> if x>nid then x else nid) 0) in
  (* Export graph to Graphvision format. *)
  export (output ^".gv.txt") (gmap graph_flow (fun (flw, cst) -> String.concat ", " [string_of_int flw; string_of_int cst])) ;
  print_endline ("Exported " ^ input ^ ".txt to Graphviz format after finding max flow min cost graph.") ;
  (* Transform graph to SVG. *)
  let ret = Sys.command ("dot -Tsvg " ^ output ^ ".gv.txt > " ^ output ^ ".svg") in
  Printf.printf ("Exported flow and cost graph to SVG (%d).\n") ret;
  let (flow, cost) = get_flow_and_cost graph_flow 0 in
  Printf.printf ("%d students were mapped to universities with total cost %d.\n") flow cost;
  ()

let () = 
    assert(Array.length Sys.argv >= 3) ;
    univ_attribution (Sys.argv.(1)) Sys.argv.(2)