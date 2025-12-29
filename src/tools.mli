open Graph

(* Copy nodes from a graph without its edges. *)
val clone_nodes: 'a graph -> 'b graph

(* Map the labels of a graph's edges. *)
val gmap: 'a graph -> ('a -> 'b) -> 'b graph

(* Adds n to the value of the arc between id1 and id2. If the arc does not exist, it is created.
   Example: add_arc g id1 id2 n *)
val add_arc: int graph -> id -> id -> int -> int graph

(* remove_arc gr id1 id2 removes the arc from id1 to id2 from the graph gr if it exists *)
val remove_arc: 'a graph -> id -> id -> 'a graph

val add_or_remove_arc_param: 'a graph -> id -> id -> ('a -> 'a -> 'a) -> 'a -> ('a -> bool) -> 'a graph

(* Adds n to the value of the arc between id1 and id2. If the arc does not exist, it is created.
   If the resulting arc would have a label of 0, it is instead removed. *)
val add_or_remove_arc: int graph -> id -> id -> int -> int graph

(* Randomizes the order of values in a list *)
val shuffle: 'a list -> 'a list