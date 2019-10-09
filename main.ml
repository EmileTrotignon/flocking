open Graphics
open Vector
open Printf
   
let iof = int_of_float
let foi = float_of_int

type bird = {pos: float * float; spe: float * float}

(* Global variables *)

let x_def = 1920 
let y_def = 1080

let speed = 2.
let rt = ref 40.
let rc = ref 5.

let bg_color = 0x444444
let fg_color = ref 0xeeeeee

(* all the following variables should stay in the range [0, 1]. *)
let conformity_dir = ref 0.9
let repulsion_attraction_balance = ref 0.97
let dir_pos_balance = ref 0.95
let noise_importance = ref 0.1

(* Functions *)
          
let print_bird b =
  match b with
  | {pos=(xp, yp); spe=(xs, ys)} ->
     printf "{pos=(%F, %F); spe=(%F, %F)} " xp yp xs ys
  
let print_bird_array barr =
  Array.iter print_bird barr
   
let insert_in_array a e i =
  for j = 1 to i do
    a.(j - 1) <- a.(j);
  done;
  a.(i) <- e

   
let ncloser_bird b barr n =
  let r = Array.sub barr 0 n in
  let s_insert b' =
    let j = ref 1 in
    let bo = ref true in
    while !j < n - 1 || !bo do 
      if (distance b.pos r.(!j).pos) > (distance b'.pos r.(!j + 1).pos) then
        (insert_in_array r b' !j;
         bo := false);
      j := !j + 1;
    done
  in
  for i = n to (Array.length barr) do
    s_insert barr.(i);
  done;
  r

let distance' (x, y) (x', y') =
  let delta_mod a a' def = min (abs_float (a -. a'))
               (min (abs_float (a -. a' +. (foi def)))
                  (abs_float (a -. a' -. (foi def))))
  in
  let dx = delta_mod x x' x_def in
  let dy = delta_mod y y' y_def in
  norm (dx, dy)
    
  
let closer_bird b barr =
  let c = ref (if barr.(0) != b then barr.(0) else barr.(1)) in
  for i  = 1 to (Array.length barr) - 1 do
    if barr.(i) != b && (distance' barr.(i).pos b.pos) < (distance' (!c).pos b.pos) then
      c := barr.(i);
  done;
  !c
  
          
let birds_in_radius b barr rad =
  let birp = Array.copy barr in
  let i = ref 0 in
  Array.iter (function b' -> (if (distance' b.pos b'.pos <= rad) && b' != b
                              then (birp.(!i) <- b';  i := !i + 1))) barr;
  (Array.sub birp 0 (!i))
    
let avg_spe barr =
  let t = Array.fold_left ( +% ) (0., 0.) (Array.map (function {pos = _; spe = s} -> s) barr) in
  (1. /. (foi (Array.length barr))) *% t

let avg_angle barr = 
  let t = Array.fold_left ( +. ) 0. (Array.map (function {pos = _; spe = s} -> angle s)
                                             barr)
  in
    (1. /. (foi (Array.length barr))) *. t
    
let avg_pos barr =
  let t = Array.fold_left ( +% ) (0., 0.) (Array.map (function {pos = p; spe = _} -> p) barr) in
  (1. /. (foi (Array.length barr))) *% t

  
let euler f b =
  {pos = b.pos +% b.spe; spe = b.spe +% f}

let random_bird () =
  {pos = (Random.float (foi x_def), (Random.float (foi y_def)));
   spe = speed *%
           unit_vector ((Random.float 10.) -. 5.,
                        (Random.float 10.) -. 5.)}

let random_bird_array n_birds =
  Array.map random_bird (Array.make n_birds ())
       
let draw_bird {pos = (x, y); spe = _} =
  moveto (iof x) (iof y);
  fill_circle (iof x) (iof y) (iof 3.)
  (*let (sx, sy) = 15. *% (Vector.unit_vector s) in 
  rlineto (- iof sx) ( - iof sy)*)
  
let draw_bird_array ba =
  set_color bg_color;
  fill_rect 0 0 x_def y_def;
  set_color !fg_color;
  (* fg_color := (1 + 256 + (256 * 256)) + !fg_color; *)
  Array.iter draw_bird ba
       
let next_bird barr b =
  let tribe = birds_in_radius b barr (!rt) in
  let tribe_spe = if Array.length tribe != 0 then avg_spe tribe else (0., 0.) in
  let tribe_pos = if Array.length tribe != 0 then avg_pos tribe else b.pos in
  let col_pos = (closer_bird b barr).pos in
  let noise = (random_bird ()).spe in
  let npos = vector_in_range (b.pos +% b.spe) (0., 0.) (foi x_def, foi y_def) in
  let nspe_dir = speed *% unit_vector (vector_mean b.spe tribe_spe !conformity_dir) in
  let repulsion' = (1. /. ((distance b.pos col_pos) /. !rc)) ** 2. in
  let repulsion = if repulsion' > 1. then repulsion' else 1. in
  let repulsion_vector =  (repulsion *% unit_vector (b.pos -% col_pos)) in
  let attraction_vector = tribe_pos -% b.pos in
  let nspe_pos = vector_mean repulsion_vector attraction_vector !repulsion_attraction_balance in
  let nspe_no_noise = (vector_mean nspe_dir nspe_pos !dir_pos_balance) in
  let nspe = (vector_mean noise nspe_no_noise !noise_importance) in
  {pos = npos; spe= nspe}
  
let birds_fly n_birds =
  
  let bird_array = ref (random_bird_array n_birds) in
  let b = ref true in
  let i = ref 0 in
  while !b do
    (*Unix.sleepf (1. /. ((foi n_birds) *. 3.) );*)
    draw_bird_array !bird_array;
    (*let s = wait_next_event [Key_pressed; Button_down] in
    if s.keypressed then (
      if s.key = 'Q' then b := false);*)
    bird_array := Array.map (next_bird !bird_array) !bird_array;
    i := !i + 1
  done;
  
;;
open_graph (String.concat "" [" "; (string_of_int x_def); "x"; (string_of_int y_def);]);
set_color bg_color;
fill_rect 0 0 x_def y_def;
birds_fly 200;
exit 0
;;
