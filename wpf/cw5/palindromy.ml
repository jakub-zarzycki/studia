(*find longest palindrom in a word*)
let palindrom word = 
    let rec maxprefix l1 l2 ret = 
        match l1, l2 with 
        | [], _ | _, [] -> ret
        | h1::t1, h2::t2 -> if h1 = h2 then maxprefix t1 t2 (ret + 1)
    in
    let rec helper l r m = 
        match r with 
        | [] -> m
        | h::t -> 
            let m = max m (max (maxprefix l r 0)  maxprefix l t 0) in
            helper l t m
    in 
        helper [] l 0
;;
