open Ford_fulkerson
open Graph

let () = assert (max_flow empty_graph 0 1 = empty_graph) ;