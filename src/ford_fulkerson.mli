open Graph

(* Find the shortest path between two nodes in a graph. *)
val find_path: 'a graph -> id -> id -> 'a arc list

(* Calculate max flow graph for an input graph. *)
val max_flow: int graph -> id -> id -> int graph