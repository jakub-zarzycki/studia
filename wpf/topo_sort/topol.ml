(* 
 * written by: Jakub Zarzycki
 * review    : Alicja Ziarko
*)

exception Cykliczne;;

(*
 * new idea:
 * use PMap to keep track of visited nodes
 * travese it as int graph
 * i thnk complexity is better as well
 * it's O(n^2) instead of O(n^2) now
 * not a lot but it's something, lol
 * those factors of 3n
 *)
let topo_sort neighbours (graph : ('a * 'a list) list) =
    let sorted = ref []
    (* maps nodes to states: *)
    (* Not_found : to do; 0 : doing; 1 : done *)
    and visited = ref PMap.empty
    in
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
                            List.iter add children
                     with
                        Not_found -> ());

                    sorted := v::!sorted;
                    visited := PMap.add v 1 !visited
        in
            (* should have used fold_left once again *)
            List.iter (fun (x,l) -> add x) graph;
            !sorted
;;

(* 
 * maps v to graph[v]
 * i should have used fold_left instead of iter
 *)
let find_neighbours graph =
    let neighbours = ref PMap.empty
    in
        List.iter (fun (v, l) -> neighbours:= PMap.add v l !neighbours)
                  graph;

        !neighbours
;;

let topol (graph : ('a * 'a list) list) =
    topo_sort (find_neighbours graph) graph
;;


