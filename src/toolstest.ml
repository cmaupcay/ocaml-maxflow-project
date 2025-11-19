open Gfile
open Tools

let () =

  let gr_test1 = clone_nodes (from_file "graphs/graph1.txt") in 
  write_file "./outfile" gr_test1;

  write_file "./outfile1" (from_file "graphs/graph1_nodes.txt");
  assert (gr_test1 = (from_file "graphs/graph1_nodes.txt"));