(*kwadratowo*)
let p f n =
let rec pb sum f a b n max len =
  if b = n then (max, len)
  else 
    let sum = sum + f (b + 1) in
    if max < sum then pb f a sum (b+1) n sum (b-a+1)
    else pb f a sum (b+1) n max len

let rec pa f a n max len = 
  if a > n then len
  else
    let sum = f a
    let (max, len) = if sum > max then (sum, 1) else (max, len) in
    let (max, len) = pb f a sum a n max len in
    pa f (a + 1) n max len
    
(*liniowo*)
(*idea: for each a check what's max sum starting at a
*       if new element > max and new element > sum so far + new element start over, else continue with adding*)
(*testcase:
*1 -1 2 -1 3
*1 -1 -1 -1 -1 -1 2*)
let p f n =
  let rec 
