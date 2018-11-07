(*some operations on lists*)
open List;;

(*list of lists -> list of heads*)
(*using map and flatten*)
let heads l =
    flatten (map (function | [] -> [] | h::t -> [h])  l)
;;

(* **************
** list is interesting if no ajoined elements are same
** divide list into longest possible interesting sublists*)
(*helper for checking if we should start a new fragment or continue*)
let f (acc, lst) h =
    if hd lst = h then 
        (rev lst::acc, [h])
    else
        (acc, h::lst)
;;

let interesting l =
    match l with
    | [] -> [[]]
    | h:: t ->
        let (acc, l) = fold_left f ([], [h]) t in
            rev (rev l::acc)
;;

(*hills and valleys*)
(*valley is index i such that 
max (left side) and max (right side) are greater than value in i*)
(*depth of valley i is 
min (max (left side of i) - value in i, max (right side) - value in i)*)
(*find the depth of the deepest valley*)
let f acc x =
    match acc with
    | [] -> [x]
    | h::t -> (max x h)::acc
;;

let max_helper l =
    fold_left f [] l
;;

let max_left l = 
    rev (max_helper l)
;;

let max_right l =
    max_helper (rev l)
;;

let rec max_depth_helper max_l max_r l depth =
    match max_l, l, max_r with
    | _, _, [] -> depth
    | h_max_l::t_max_l, h_l::t_l, h_max_r::t_max_r ->
        let depth_new = min (h_max_l - h_l) (h_max_r - h_l) in 
            max_depth_helper t_max_l t_l t_max_r (max depth depth_new)
    | _ -> failwith "impossible"
;;

let max_depth l =
    let max_r = max_right l 
    and max_l = max_left l in
        match l, max_r with
        | [], _ | _, [] -> 0
        | _, h::[] -> 0
        | _::t_l, _::_::t_max_r -> max_depth_helper max_l t_l t_max_r 0
;;


