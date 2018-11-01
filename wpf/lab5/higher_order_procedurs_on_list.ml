(*implementation of some operations on lists*)

(*fold left, 'cause apparently it's not default in ocaml*)
let rec fold_left f a l =
    match l with
    | [] -> a
    | h::t -> fold_left f (f a h) t
;;

(*same for fold_right*)
let rec fold_right f l a =
    match l with
    | []   ->a
    | h::t ->f h (fold_right f t a)
;;

(*flatten list of lists using fold left*)
let flatten_l l =
    let append l elem =
        l @ elem
    in
        fold_left append [] l
;;

(*flatten list of lists using fold right*)
let flatten_r l =
    let append l elem =
        elem @ l
    in 
        fold_right append [] l
;;

(*n-th element of list using fold left*)
let elem_l l n =
    fold_left (fun n x -> if n = 0 then x else (n-1)) n 
;;

