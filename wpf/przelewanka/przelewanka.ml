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

let split a = 
    let n = Array.length a
    in 
        (Array.init n (fun i -> fst a.(i)), Array.init n (fun i -> snd a.(i)))
;;

let rec gcd x y =
    if y = 0 then x
    else gcd y (x mod y)
;;

let pour volumes state i j =
    let tmp = Array.copy state
    in 
        tmp.(i) <- tmp.(i) + tmp.(j);
        tmp.(j) <- max (tmp.(i) - volumes.(i)) 0;
        tmp.(i) <- min tmp.(i) volumes.(i);
        tmp
;;

let fill volumes state i =
    let tmp = Array.copy state
    in 
        tmp.(i) <- volumes.(i);
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

let states_eq x y =
    let res = ref true 
    in
        for i = 0 to Array.length x - 1 do
            if x.(i) <> y.(i) then res := false;
            Printf.printf "%d ? %d\n" x.(i) y.(i)
        done;
        if !res then failwith "rowne"
        else !res
;;

(* solver for przelewanka *)
let przelewanka_solver condition =
    let no_of_glasses = Array.length condition in
    let source = Array.make no_of_glasses 0 in
    let volumes, target = split condition
    and visited = ref (Set.add source Set.empty)
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
        and fill = fill volumes state 
        and pour = pour volumes state 
        and empty = empty state
        in   
            for i = 0 to no_of_glasses - 1 
            do
                for j = 0 to no_of_glasses - 1 
                do 
                    if i = j then
                        let state1 = fill i
                        and state2 = empty i
                        in 
                            ret := state1::state2::!ret
                    else
                        let state = pour i j 
                        in
                            ret := state::!ret
                done
            done;
            !ret
    in 
    
    (* start BFS *)
    let current_step = ref (Queue.create ())
    and next_step = ref (Queue.create ())
    and path_length = ref 0
    in
        Queue.push source !current_step;
                
        while true do
            let state = Queue.pop !current_step
            in
                if (states_eq state target) then raise (Result (!path_length))
                else 
                    (let new_states = 
                            List.filter 
                                    (fun s -> not (Set.mem s !visited))
                                    (neighbours state)
                    in 
                        List.iter 
                                (fun s -> 
                                    Queue.push s !next_step;
                                    Printf.printf "pushing: ";
                                    states_eq s target;
                                    Printf.printf "\n";
                                    visited := Set.add s !visited) 
                                new_states);
                                
                    (if Queue.is_empty !current_step then
                        current_step := !next_step;
                        next_step := Queue.create ();
                        path_length := !path_length + 1);
                    Printf.printf "%d\n" !path_length
        done;
        -1
;;

let przelewanka condition =
    try przelewanka_solver condition 
    with
        | Result (result) -> result
;;
        
        
