let silnia n =
let rec silnia_pom n pom =
  if n = 0 then pom
  else silnia_pom (n - 1) (pom * n)
in
  silnia_pom n 1
;;

