(* 
 * written by: Jakub Zarzycki
 * review    : Alicja Ziarko
*)

exception Cykliczne;;

(*
 * plan: 
 * translate 'a to int, sort int, translate int to 'a
 *)  

(*
 * topological sort for iniegers
 *)
let topo_sort encoder (graph : ('a * 'a list) array) =
    let sorted = ref []
    (* in visited: -1 : to do; 0 : doing; 1 : done *)
    and visited = Array.make (Array.length graph) (-1)
    in
        let visit v_encoded = 
                visited.(v_encoded) <- visited.(v_encoded) + 1
        in
            let rec add v =
                let v_encoded = PMap.find v encoder 
                in
                    if visited.(v_encoded) = 0 then raise Cykliczne
                    else if visited.(v_encoded) < 0 then 
                        visit v_encoded;
                        
                        (match graph.(v_encoded) with
                        | _, [] -> ()
                        | _, children -> List.iter add children);
                        
                        if visited.(v_encoded) = 0 
                        then 
                            sorted := v_encoded::!sorted;
                            visit v_encoded
            in
                for i = 0 to Array.length graph - 1 do
                    add (fst graph.(i))
                done;
                !sorted
;;

(*
 * translates list of adjacency lists to
 * array of adjacency lists
 *)
let encode g = 
    Array.of_list g
;;

(*
 * translate int graph to abstract graph
 *)
let decode decoder l =
    List.rev (List.fold_left (fun acc x -> decoder.(x)::acc) [] l)
;;

let make_decoder graph =
    (* fst List.hd graph ensures type correctness *)
    let decoder = Array.make (List.length graph) (fst (List.hd graph)) 
    in
        let i = ref 0 
        in
            List.iter (fun x -> 
                        decoder.(!i) <- (fst x);
                        i := !i + 1)
                      graph;
            decoder
;;

let make_encoder decoder =
    let encoder = ref (PMap.create compare)
    in
        for i = 0 to Array.length decoder - 1 do
            encoder := PMap.add (decoder.(i)) i !encoder;
        done;
        !encoder
;;

let topol (graph : ('a * 'a list) list) =
    let decoder = make_decoder graph in
    let encoder = make_encoder decoder
    in
        decode decoder (topo_sort encoder (encode graph))
;;

let graf = [[(1, [2]); (1, [3])]];;

let test = topol graf;;

