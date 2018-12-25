open Complex

let ( +% ) (x1, y1) (x2, y2) =
  (x1 +. x2), (y1 +. y2)

let ( -% ) (x1, y1) (x2, y2) =
  (x1 -. x2), (y1 -. y2)

let ( ~-% ) (x, y) =
  (-.x, -.y)
  
let ( *% ) f (x, y) =
  f *. x, f *. y

let ( /% ) f (x, y) =
  (f /. x, f /. y)

let norm (x, y) =
  hypot x y

let snorm (x, y) =
  (x *. x +. y *. y)
  
let distance u v =
  abs_float (norm (u -% v))
  
let mean_list lv =
  List.fold_left ( +% ) (0., 0.) lv

let unit_vector v =
  (1. /.(norm v)) *% v

let mod_vector (x, y) (x', y') =
  (mod_float x x', mod_float y y')

let rec float_in_range x b1 b2 =
  let d = b2 -. b1 in
  if x <= b2 && x >= b1 then
    x
  else if x < b1 then
    float_in_range (x +. d) b1 b2
  else float_in_range (x -.d) b1 b2
  
let vector_in_range (x, y) (xb1, yb1) (xb2, yb2) =
  (float_in_range x xb1 xb2, float_in_range y yb1 yb2)

let float_mean f1 f2 coeff =
  let coeff' = 1. -. coeff in
  (f1 *. coeff +. f2 *. coeff') 
  
let vector_mean (x1, y1) (x2, y2) coeff =
  let x = float_mean x1 x2 coeff in
  let y = float_mean y1 y2 coeff in
  (x, y)
  
let angle (x, y) =
  arg {re= x; im=y}
  
