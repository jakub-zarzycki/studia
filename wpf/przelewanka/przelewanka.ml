(* 
 * written by: Jakub Zarzycki
 * review  by: ______________
 *)

(* exceptions to return from function early *)
exception Result of int

(*
 * we treat states of glasses as nodes in directed graph
 * then our problem is equivalent to finding shortes path between 
 * state (0, 0, ..., 0) and required state
 *)

let condition = Array.make 2 (1, 1);;

let pour state i j =
    let tmp = Array.copy state
    in 
        tmp.(i) <- tmp.(i) + tmp.(j);
        tmp.(j) <- max (tmp.(i) - (fst condition.(i))) 0;
        tmp.(i) <- min tmp.(i) (fst condition.(i));
        tmp
;;

let fill state i =
    let tmp = Array.copy state
    in 
        tmp.(i) <- fst condition.(i);
        tmp
;;

let empty state i =
    let tmp = Array.copy state
    in 
        tmp.(i) <- 0;
        tmp
;;

(* set to use as frontiner *)
module Set = Set.Make (struct type t = int array let compare = compare end);;

(* wrapper for przelewanka, ugly code goes here *)
let przelewanka_wrapper condition =
    let no_of_glasses = Array.length condition in
    let target = Array.make no_of_glasses 0
    in

    (* list of neighbours in graph of states *)
    (* try doing it with generators *)
    (* neighbours : int array -> int array list *)
    (* running time O(number of possible states in glasses) *)
    (* so O(k^n) where k is size of biggest glass *)
    let neighbours state =
        (* return list because i can't use generators *)
        let ret = ref []
        
        (* partially execute those *)
        and fill = fill state 
        and pour = pour state 
        and empty = empty state
        in
        
            for i = 0 to no_of_glasses - 1 
            do
                for j = 0 to no_of_glasses - 1 
                do 
                    if i = j then
                        ret := (fill i)::(empty i)::!ret
                    else
                        ret := (pour i j)::!ret
                done
            done;
            !ret
    in 
    
    (* start BFS *)
    let path_length = ref 0
    and frontiner = ref (Set.add (Array.make no_of_glasses 0) Set.empty)
    in
        while true do
            let new_frontiner = ref Set.empty
            in
                Set.iter 
                        (fun state ->
                                if state = target then raise (Result !path_length)
                                else begin
                                    path_length := !path_length + 1;
                                    new_frontiner := 
                                        List.fold_left 
                                            (fun x y -> Set.add y x)
                                            !new_frontiner
                                            (neighbours state) 
                                    end)
                        !frontiner;
                frontiner := !new_frontiner
        done;
        raise (Result (-1))
;;

let przelewanka condition =
    try przelewanka_wrapper condition 
    with
        | Result (result) -> result
;;
        
        
