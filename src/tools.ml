open Graph

let clone_nodes gr = n_fold gr (new_node) empty_graph

let gmap gr f = e_fold gr 
  (fun grt ar -> new_arc grt {src = ar.src ; tgt = ar.tgt ; lbl = f (ar.lbl)}) 
  (clone_nodes gr)

let add_arc gr id1 id2 n = let arc12 = find_arc gr id1 id2 in 
  match arc12 with
  |None -> new_arc gr {src=id1 ; tgt=id2 ; lbl = n}
  |Some ar -> new_arc gr {src=id1 ; tgt=id2 ; lbl = n+ar.lbl}