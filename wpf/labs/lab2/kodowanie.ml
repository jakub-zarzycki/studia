(*works only for a, b >= 0*)
let encode a b =
  let rec pom a b c = 
    if a = 0 && b = 0 then c
    else pom (a / 10) (b / 10) (100 * c + 10 * (a mod 10) + (b mod 10))
  in
  pom a b 0
;;

(*works only for c >= 0*)
let decode c =
  let rec pom a b c =
    if c = 0 then (a, b)
    else pom (10 * a + (c mod 100) / 10) (10 * b + (c mod 10)) (c / 100)
  in
  pom 0 0 c
;;
