(*infix and suffix list of nodes*)
(*recreate tree*)
type 'a tree = Leaf | Node of 'a tree * 'a * 'a tree

let recreate infix prefix =
    let rec make_to x infix prefix =
        match prefix with
        | [] -> failwith "prefix list too short"
        | h::t -> 
            if x = h 
            then (Leaf, infix, prefix)
            else 
                (match infix with
                | [] -> failwith "infix list too short"
                | hp::t -> 
                    let (left, left_infix, left_prefix) =
                        make_to hp infix t
                    in 
                        (match left_infix with
                        | [] -> failwith "random error message"
                        | h::t -> 
                            let (right, right_infix, right_prefix) =
                                make_to h t left_prefix 
                            in
                                (
                                    Node (left, hp, right), 
                                    right_infix, 
                                    right_prefix
                                )))
    in
        let (t, _, _) = make_to (-1) infix prefix in t
;;

