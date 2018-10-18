(*length of max sum subarray in linear time*)
let pm l =
    let rec pom len sum maxlen maxsum l =
        match l with 
        | [] -> len
        | h::t ->
            let sum, len = if h > (sum + h) then (h, 1) else ((sum + h), (len + 1)) in
                if sum > maxsum then pom len sum len sum t
                else pom len sum maxlen maxsum t
    in
        match l with
        | [] -> 0
        | h::t -> pom 1 h 1 h t
;;
