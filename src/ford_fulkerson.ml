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

(* Bellman-Ford queue algorithm to find minimum cost path between src and tgt in gr *)
let bellman_ford gr src tgt = 

  (* Main loop updates an array with the predecessor and cost of every node.
     Yes, dynamic programming in ocaml is prob
ably not the best idea, but I'm not enough of a graph master to figure out an alternative. *)
  let rec loop gr src acu = function
    (* If the queue is empty, terminate the algorithm *)
    |[]->acu

    (* Look through node x's outward arcs *)
    |x::r -> let queue = List.fold_left (
          (* If it's cheaper to reach an arc's destination via x than the current stored path,
             update that destination's entry in the array *)
          fun queue_temp ar -> if (snd (Array.get acu ar.tgt) <= snd (Array.get acu ar.src) + snd ar.lbl)
            then queue_temp
            else (Array.set acu ar.tgt (ar.src, snd (Array.get acu ar.src) + snd ar.lbl);
              (* If an updated node isn't in the queue, add it *)
              if (List.mem ar.tgt queue_temp) then queue_temp else (List.rev (ar.tgt::(List.rev queue_temp))))
          ) r (shuffle (out_arcs gr x))
      in loop gr src acu queue 

  in

  (* This goes through the array, starting fro
m node tgt's entry,
     and recursively visits predecessors to construct a path *)
  let rec path_from_array gr node_array acu tgt = 
    (* If we have reached the source or the target could not be reached, exit the loop *)
    if (tgt=src || snd (Array.get node_array tgt) = max_int) then acu
    (* Otherwise loop on the target's predecessor *)
    else let src_cur = fst (Array.get node_array tgt) in 
      match find_arc gr src_cur tgt with
        |None->[]
        |Some ar -> path_from_array gr node_array (ar::acu) src_cur
  in

  (*Check that the source and target node exist in the graph*)
  match (node_exists gr src) && (node_exists gr tgt) with
    |false->[]
    (*If they exist, start the loop with the initial array and parse a path from the result*)
    |true-> 
      (* Initial array has infinite cost to reach every node, except the source which has cost 0 *)
      let list_init = Array.make (1 + (n_fold gr (fun x nid -> if x>nid then x else nid) 0)) (src, max_int) in

      Array.set list_init src (src, 0);
      path_from_array gr (loop gr src list_init [src]) [] tgt

(* Main algorithm for Ford-Fulkerson maximum flow search. 
Produces a flow graph with maximum flow from node src to node tgt. *)
let max_flow gr src tgt = 
  
  (* This main loop produces a difference graph ("graphe d'écart") based on the initial capacity graph *)
  let rec loop gr src tgt = 
    (* We try to find a path from src to tgt in the difference graph *)
    match find_path gr src tgt with
      (* If there are no paths left, we have achieved maximum flow *)
      |[]-> gr
      (* If we found a path, we update the difference graph *)
      (* First we find the minimum amount of flow we can add that saturates an arc *)
      |l-> let min_cap = List.fold_left (
        fun cur_min ar -> if cur_min > ar.lbl then ar.lbl else cur_min)
        max_int l in 
        (* Then we update all the forward and backward arcs in the difference graph.
           We remove arcs that would have a label of 0 so as to not find invalid paths later. *)
        loop (List.fold_left (
          fun cur_gr ar -> add_or_remove_arc (add_or_remove_arc cur_gr ar.src ar.tgt (-min_cap)) ar.tgt ar.src min_cap) 
        gr l) src tgt
  in 

  let gr_ecart = loop gr src tgt in 

  (* Now we convert the difference graph to a flow graph by comparing it with the capacity graph *)
  e_fold gr (
    fun g a -> match find_arc gr_ecart a.src a.tgt with
      (* If an arc was saturated *)
      |None -> new_arc g {src=a.src ; tgt=a.tgt ; lbl=a.lbl}
      (* This means an opposite arc between the same nodes should be there *)
      |Some ar_ec when ar_ec.lbl >= a.lbl -> g
      (* If an arc has some flow but is not saturated or had an opposite arc in the capacity graph *)
      |Some ar_ec -> new_arc g {src=a.src ; tgt=a.tgt ; lbl=(a.lbl - ar_ec.lbl)}
  ) (clone_nodes gr)


(* Main algorithm for  maximum flow search. 
   Produces a flow graph with maximum flow and minimal cost from node src to node tgt. *)
let max_flow_min_cost gr_base src tgt = 

  let add_or_remove_arc_pair gr src tgt pair =
    add_or_remove_arc_param gr src tgt (
    fun (f1,c1) (f2, _c2) -> (f1+f2,c1)
    ) pair (fun (x,_) -> x=0) 
  in
  
  (* This main loop produces a difference graph ("graphe d'écart") based on the initial capacity graph *)
  let rec loop gr src tgt = 
    (* We try to find a minimum cost path from src to tgt in the difference graph *)
    match bellman_ford gr src tgt with
      (* If there are no paths left, we have achieved maximum flow *)
      |[]-> gr

      (* If we found a path, we update the difference graph *)
      (* First we find the minimum amount of flow we can add that saturates an arc *)
      |l-> 
        let min_cap = List.fold_left (
        fun cur_min ar -> if cur_min > fst ar.lbl then fst ar.lbl else cur_min)
        max_int l in 
        (* Then we update all the forward and backward arcs in the difference graph.
           We remove arcs that would have a label of 0 so as to not find invalid paths later. *)
        loop (List.fold_left (
          fun cur_gr ar -> add_or_remove_arc_pair (add_or_remove_arc_pair cur_gr ar.src ar.tgt (-min_cap, snd ar.lbl)) ar.tgt ar.src (min_cap, 0-(snd ar.lbl))) 
        gr l) src tgt
  in 

  let gr_ecart = loop gr_base src tgt in 

  (* Now we convert the difference graph to a flow graph by comparing it with the capacity graph *)
  e_fold gr_base (
    fun g a -> match find_arc gr_ecart a.src a.tgt with
      (* If an arc was saturated *)
      |None -> new_arc g {src=a.src ; tgt=a.tgt ; lbl=a.lbl}
      (* This means an opposite arc between the same nodes should be there *)
      |Some ar_ec when fst ar_ec.lbl >= fst a.lbl -> g
      (* If an arc has some flow but is not saturated or had an opposite arc in the capacity graph *)
      |Some ar_ec -> new_arc g {src=a.src ; tgt=a.tgt ; lbl=((fst a.lbl) - (fst ar_ec.lbl), snd a.lbl)}
  ) (clone_nodes gr_base)

(* Get the flow and cost going through gr with source src *)
let get_flow_and_cost gr src =
  (* Sum up the flow going from src *)
  (List.fold_left (fun acu ar -> acu + (fst ar.lbl)) 0 (out_arcs gr src),
  (* Sum up the cost of every arc *)
  e_fold gr (fun acu ar -> acu + (snd ar.lbl * fst ar.lbl)) 0)