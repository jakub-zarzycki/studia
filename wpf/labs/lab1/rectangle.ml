(* calculate intersection of 2 rectangles *)
let projection_intersection x1a x2a x1b x2b =
    let x1c = max x1a x1b
    let x2c = min x2a x2b
    (x1c, x2c);;

let intersection_of_rectangles x1a x2a y1a y2a x1b x2b y1b y2b = 
    let x1c, x2c = projection_intersection x1a x2a x1b x2b
    let y1c, y2c = projection_intersection y1a y2a y1b y2b
    (x1c,x2c, y1c, y2c);;


