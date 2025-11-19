open Gfile
open Graph
open Tools

let () =

  (*Testing clone_nodes*)
  let gr1 = from_file "graphs/graph1.txt" in

  let gr_test1 = clone_nodes gr1 in 
  write_file "./outfile" gr_test1;

  let gr_test2 = clone_nodes empty_graph in 
  write_file "./outfile1" gr_test2;

  (*Testing gmap*)
  let gr_test3 = gmap empty_graph (fun x->x) in 
  write_file "./outfile2" gr_test3;
 
  let gr2 = gmap gr1 int_of_string in

  let gr_test4 = gmap gr2 (fun s -> string_of_int (s + 12) ) in 
  write_file "./outfile3" gr_test4;

  (* Testing add_arc*)
  let gr_test5 = add_arc gr2 3 4 122 in 
  write_file "./outfile4" (gmap gr_test5 string_of_int);

  let gr_test6 = add_arc gr2 0 5 68 in 
  write_file "./outfile5" (gmap gr_test6 string_of_int);

