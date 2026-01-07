open Graph

(* Find the shortest path between two nodes in a graph. *)
val find_path: 'a graph -> id -> id -> 'a arc list

(*Find minimum cost path from source to dest in a graph*)
val bellman_ford: ('a*int) graph -> id -> id -> ('a*int) arc list

(* Calculate max flow graph between two nodes for an input graph. *)
val max_flow: int graph -> id -> id -> int graph

(* Calculate max flow graph with min cost between two nodes for an input graph. *)
val max_flow_min_cost: (int*int) graph -> id -> id -> (int*int) graph

(* Calculate the total flow and cost in a flow graph (indicating the source) created with max_flow_min_cost *)
val get_flow_and_cost: (int*int) graph -> id -> int*int