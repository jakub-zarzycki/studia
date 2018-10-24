(*duża struktura, trzeba obliczyć wynik na końcu*)
let rec pot1 x n =
  if n = 0 then 1
  else x * (pot1 x (n / 2))
;;

(*ogonowo*)
let pot2 x n =
let rec pot_pom x y n =
  if n = 0 then y
  else pot_pom x (x * y) (n - 1)
in 
  pot_pom x 1 n
;;

(*logarytmicznie*)
let pot3 x n =
let rec pom y x2 n =
  if n = 0 then y
  else if n mod 2 = 0 then pom y (x2 * x2) (n / 2)
  else let y = y * x2 in 
  pom y (x2 * x2) (n / 2)
in 
  pom 1 x n
;;
