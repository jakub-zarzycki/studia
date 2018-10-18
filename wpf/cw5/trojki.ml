(*find triples (a, b, c) such that they fulfil triangle inequality in sorted, increasing list*)
let triples l =
    let rec helpc l ret a b =
        match l with 
        | [] -> ret
        | c::t -> 
            if a + b <= c then ret
            else 
                let ret = (a, b, c)::ret in
                helpc t ret a b
    in
    let rec helpb l ret a =
        match l with 
        | [] -> ret
        | b::t -> helpb t (helpc t ret a b) a
    in
    let rec helpa l ret =
        match l with 
        | [] -> ret
        | a::t -> helpa t (helpb t ret a)
    in
    helpa l []
;;
