open Graph
open Tools

(*Breadth-first path search between nodes src and tgt in graph gr*)
let find_path gr src tgt = 

  let rec loop gr src tgt acu = 

    (*If we have reached the target, return the current path*)
    if src = tgt then acu
    else

      (*If we are in a node we have already visited, abort the search*)
      if List.exists (fun x -> src = x.src) acu then []
      else

        (*Check if the current nodes has any outward arcs*)
        match out_arcs gr src with
        |[]->[]

        (*For every outward arc, try to find a path to the destination from its target node*)
        |l-> List.fold_left (fun new_path arc -> 
          match loop gr arc.tgt tgt (arc::acu) with
            |[]->new_path
            (*If there is a path, we keep it if it's the first one or the shortest one*)
            |res -> if new_path = [] || (List.compare_lengths res new_path) < 0 then res else new_path
          ) [] l
  in

  (*Check that the source and target node exist in the graph*)
  match (node_exists gr src) && (node_exists gr tgt) with
    |false->[]
    (*If they exist, start the loop with an empty initial path and reverse the result*)
    |true-> List.rev (loop gr src tgt [])


(* Main algorithm. *)
let max_flow gr src tgt = 
  
  let rec loop gr src tgt = 
    match find_path gr src tgt with
      |[]-> gr
      |l-> let min_cap = List.fold_left (
        fun cur_min ar -> if cur_min > ar.lbl then ar.lbl else cur_min) 
        max_int l in 
        loop (List.fold_left (
          fun cur_gr ar -> add_or_remove_arc (add_or_remove_arc cur_gr ar.src ar.tgt (-min_cap)) ar.tgt ar.src min_cap) 
        gr l) src tgt
  in 

  let gr_ecart = loop gr src tgt in 

  e_fold gr (
    fun g a -> match find_arc gr_ecart a.src a.tgt with
      |None -> new_arc g {src=a.src ; tgt=a.tgt ; lbl=a.lbl}
      |Some ar_ec when ar_ec.lbl >= a.lbl -> g
      |Some ar_ec -> new_arc g {src=a.src ; tgt=a.tgt ; lbl=(a.lbl - ar_ec.lbl)}
  ) (clone_nodes gr)