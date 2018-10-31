(*f : [0, infinity) -> [0,infinity)
*     bijective
*     continus 
*     f (x + d) - f x >= d
*     f 0 = 0
*implementation of f inverse*)
(*y -> if f (b-a)/2 > y check (a, (b-a)/2) else check ((b-a)/2, b)*)
let g f =
    function y ->
        let rec binsearch upper lower =
            let middle = (lower -. upper) /. 2. in
                if (upper -. lower) < eps 
                then 
                    (upper - lower) /. 2. 
                else
                    if (f middle) < y
                    then
                        binsearch middle upper
                    else 
                        binsearch lower middle
        in 
            binsearch 0 y
;;

