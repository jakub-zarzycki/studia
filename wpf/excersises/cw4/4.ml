(*liczba różnych elementów 2 rosnących list*)
let rec ile x y =
  let rec pom (max, count) lx ly = 
    let maxp max count head = 
      if head = max then (max, count) else (head, count + 1)
    in
      match lx, ly with 
      | [], [] -> count
      | h::t, [] | [], h::t -> 
        pom (maxp max count h) [] t
      | hx::tx, hy::ty ->
        if hx < hy then pom (maxp max count hx) tx ly
        else pom (maxp max count hy) lx ty
  in
    match x, y with
    | [], [] -> 0
    | [], h::t | h::t, [] -> pom (h, 1) [] t
    | hx::tx, hy::ty -> 
      if hx < hy then pom (hx, 1) tx y
      else pom (hy, 1) x ty
;;
