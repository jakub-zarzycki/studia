type 'a tree = Node of 'a * 'a tree list

(*height of a tree*)
let height t =
    let rec helper_node t = 
        match t with Node(_, l) -> helper_list l 0 + 1
    and helper_list l h =
        match l with
        | [] -> m
        | head::tail -> helper_list tail (max (helper_node head) h)
;;

(*****************************************************
*a company is going to a party
*everyone has party-score
*if someone's going their bos and subordinates are not
*what's maximum cummulative party score?
******************************************************)
let party t =
    let rec helper_node t = 
        match t with Node(i, l) -> helper_list (i, 0) l
    and helper_list (mz, mb) l =
        match l with
        | [] -> (mz, mb)
        | head::tail -> 
            let (mzh, mbh) = helper_node h in
                helper_list (mz + mbh, mb + max mzh zbh) 
    in
        let (mz, mb) = helper_node in max mz mb
;;
