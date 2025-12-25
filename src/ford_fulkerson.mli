open Graph

(* Find the shortest path between two nodes in a graph. *)
val find_path: 'a graph -> id -> id -> 'a arc list

(*Find minimum cost path from source to all dests in a graph*)
val bellman_ford: ('a*int) graph -> id -> (id*int) array

(* Calculate max flow graph for an input graph. *)
val max_flow: int graph -> id -> id -> int graph