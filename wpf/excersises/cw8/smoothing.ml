(*function smoothing*)
let smooth dx f = 
    function x -> (f x +. f (x -. dx) +. (f +. dx)) /. 3
;;

