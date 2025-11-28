open Graph

(*Breadth-first path search between nodes src and tgt in graph gr*)
let find_path gr src tgt = 

  let rec loop gr src tgt acu = 
    if src = tgt then acu
    else
      if List.exists (fun x -> src = x.src) acu then []
      else
        match out_arcs gr src with
        |[]->[]
        |l-> List.fold_left (fun new_path arc -> 
          match loop gr arc.tgt tgt (arc::acu) with
            |[]->new_path
            |res -> if new_path = [] || (List.compare_lengths res new_path) < 0 then res else new_path
          ) [] l
in

match (node_exists gr src) && (node_exists gr tgt) with
  |false->[]
  |true-> List.rev (loop gr src tgt [])



(* Main algorithm. *)
let max_flow _gr _src _tgt = empty_graph