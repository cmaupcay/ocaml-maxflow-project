open Gfile
open Graph
open Tools

let () =

  (*Testing clone_nodes*)

  let gr1 = from_file "graphs/base/graph1.txt" in

  let gr_test1 = clone_nodes gr1 in 
  write_file "./graphs/outfiles/outfile1" gr_test1;

  let gr_test2 = clone_nodes empty_graph in 
  write_file "./graphs/outfiles/outfile2" gr_test2;

  (*Testing gmap*)

  let gr_test3 = gmap empty_graph (fun x->x) in 
  write_file "./graphs/outfiles/outfile3" gr_test3;
 
  let gr2 = gmap gr1 int_of_string in

  let gr_test4 = gmap gr2 (fun s -> string_of_int (s + 12) ) in 
  write_file "./graphs/outfiles/outfile4" gr_test4;

  (* Testing add_arc*)

  let gr_test5 = add_arc gr2 3 4 122 in 
  write_file "./graphs/outfiles/outfile5" (gmap gr_test5 string_of_int);

  let gr_test6 = add_arc gr2 0 5 68 in 
  write_file "./graphs/outfiles/outfile6" (gmap gr_test6 string_of_int);

  (* Testing remove_arc *)

  let gr_test7 = remove_arc gr2 0 3 in
  write_file "./graphs/outfiles/outfile7" (gmap gr_test7 string_of_int);

  let gr_test8 = remove_arc gr2 0 4 in 
  write_file "./graphs/outfiles/outfile8" (gmap gr_test8 string_of_int);

  (* Testing add_or_remove_arc *)

  let gr_test9 = add_or_remove_arc gr2 0 3 130 in
  write_file "./graphs/outfiles/outfile9" (gmap gr_test9 string_of_int);

  let gr_test10 = add_or_remove_arc gr2 0 3 (-10) in
  write_file "./graphs/outfiles/outfile10" (gmap gr_test10 string_of_int);

  let gr_test11 = add_or_remove_arc gr2 0 4 150 in
  write_file "./graphs/outfiles/outfile11" (gmap gr_test11 string_of_int);

  let gr_test12 = add_or_remove_arc gr2 0 4 0 in
  write_file "./graphs/outfiles/outfile12" (gmap gr_test12 string_of_int);