(*lista od 1 do n*)
let rec lista n =
  let rec pom n l =
    if n = 0 then []
    else pom (n - 1) (n::l)
  in
    pom n []
;;
