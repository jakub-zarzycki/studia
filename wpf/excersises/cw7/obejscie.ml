type 'a tree = Node of 'a tree * 'a * 'a tree | Leaf

(*obejscie drzewa takie, że dwa kolejne nody są co najwyżej 3 od siebie*)
let obejdz t =
    let rec obejscie od_korzenia t acc =
        match t with 
        | Leaf -> acc
        | Node(l, a, r) -> 
            if od_korzenia then
                let acc = if od_korzenia them acc else a::acc in
                let acc = obejscie (not od_korzenia) l acc in 
                let acc = obejscie (not od_korzenia) r acc in
                    if od_korzenia then a::acc else acc
    in
        obejscie true t []
;;
