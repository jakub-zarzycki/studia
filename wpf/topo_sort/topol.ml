(* 
 * written by: Jakub Zarzycki
 * review    : Alicja Ziarko
*)

exception Cykliczne;;

(* 
 * maps v to graph[v]
 * returns ('a 'a list) PMap
 * O(V) running time
 * (O(#veritces with non-empty adjastency lists))
 * O(V) space
 *)
let find_neighbours graph =
    List.fold_left (fun acc (v, l) -> PMap.add v l acc) PMap.empty graph
;;

(*
 * topologicallt sort graph graph with adjastency list 
 * neighbours = find_neighbours graph
 * O(V + E) running time
 * O(V) space
 *)
let topo_sort neighbours (graph : ('a * 'a list) list) =
    let sorted = ref []
    (* maps nodes to states: *)
    (* Not_found : to do; 0 : doing; 1 : done *)
    and visited = ref PMap.empty
    in
        (* changes visited and sorted *)
        let rec add v =
            try 
                let state = PMap.find v !visited
                in 
                    if state = 0 then raise Cykliczne
            with
                Not_found ->
                    visited := PMap.add v 0 !visited;

                    (try 
                        let children = PMap.find v neighbours
                        in
                            (* changes sorted and visited *)
                            List.iter add children
                     with
                        Not_found -> ());

                    sorted := v::!sorted;
                    visited := PMap.add v 1 !visited
        in
            (* changes sorted and visited *)
            List.iter (fun (x, l) -> add x) graph;
            !sorted
;;

(*
 * topologically sort graph
 * it's jsut wrapper for topo_sort so
 * O(V + E) running time
 * O(V) space
 *)
let topol (graph : ('a * 'a list) list) =
    topo_sort (find_neighbours graph) graph
;;

