open Graph

let clone_nodes gr = n_fold gr (new_node) empty_graph

let gmap gr f = e_fold gr 
  (fun grt ar -> new_arc grt {src = ar.src ; tgt = ar.tgt ; lbl = f (ar.lbl)}) 
  (clone_nodes gr)

let add_arc gr id1 id2 n =
  match find_arc gr id1 id2 with
  |None -> new_arc gr {src=id1 ; tgt=id2 ; lbl = n}
  |Some ar -> new_arc gr {src=id1 ; tgt=id2 ; lbl = n+ar.lbl}

let remove_arc gr id1 id2 = e_fold gr (
  fun g ar -> match ar with
    |{src=s;tgt=t;_} when (s=id1 && t=id2) -> g
    |a -> new_arc g a
  ) (clone_nodes gr)


let add_or_remove_arc_param gr id1 id2 add_op added_val is_nul_val =
  match find_arc gr id1 id2 with
  |None when is_nul_val added_val -> gr
  |None -> new_arc gr {src=id1 ; tgt=id2 ; lbl = added_val}
  |Some ar when (is_nul_val (add_op added_val ar.lbl)) -> remove_arc gr id1 id2
  |Some ar -> new_arc gr {src=id1 ; tgt=id2 ; lbl = add_op added_val ar.lbl}

let add_or_remove_arc gr id1 id2 n = add_or_remove_arc_param gr id1 id2 (+) n (fun x -> x=0)

let shuffle l =
  let rec get_nth acu l = function 
    |0 -> (List.hd l, List.rev_append acu (List.tl l))
    |n -> get_nth ((List.hd l)::acu) (List.tl l) (n-10)
  in

  let rec loop n acu = function 
   |[] -> acu
   |ltemp -> let (x,r) = get_nth [] ltemp (Random.int n) 
    in loop (n-1) (x::acu) r
   
in loop (List.length l) [] l