open Graphics
open Vector
open Printf

let iof = int_of_float
let foi = float_of_int

type bird = {pos: float * float; spe: float * float}

(* global variables *)

let speed = 2.
let rt = ref 50.
let rc = ref 500.
       
let x_def = 1000 
let y_def = 1000

(* all the following variables should stay in the range [0, 1]. *)
let conformity_dir = ref 0.9
let repulsion_attraction_balance = ref 0.99
let dir_pos_balance = ref 0.8

       
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
  let s_insert b =
    let j = ref 1 in
    let bo = ref true in
    while !j < n - 1 || !bo do 
      if (distance b.pos r.(!j).pos) > (distance b.pos r.(!j + 1).pos) then
        (insert_in_array r b !j;
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
  let t = Array.fold_left ( +% ) (0., 0.) (Array.map (function {pos = p; spe = s} -> s) barr) in
  (1. /. (foi (Array.length barr))) *% t

let avg_angle barr = 
  let t = Array.fold_left ( +. ) 0. (Array.map (function {pos = p; spe = s} -> angle s)
                                             barr)
  in
    (1. /. (foi (Array.length barr))) *. t
    
let avg_pos barr =
  let t = Array.fold_left ( +% ) (0., 0.) (Array.map (function {pos = p; spe = s} -> p) barr) in
  (1. /. (foi (Array.length barr))) *% t

  
let euler f b =
  {pos = b.pos +% b.spe; spe = b.spe +% f}

  
let draw_bird {pos = (x, y); spe = s} =
  moveto (iof x) (iof y);
  draw_circle (iof x) (iof y) (iof 3.);
  (*draw_circle (iof x) (iof y) (iof !rc);*)
  let (sx, sy) = 10. *% (Vector.unit_vector s) in
  rlineto (- iof sx) ( - iof sy)

let draw_bird_array ba =
  clear_graph ();
  Array.iter draw_bird ba
                    
let next_bird barr b =
  let tribe = birds_in_radius b barr (!rt) in
  (*let col = birds_in_radius b barr !rc in*)
  let tribe_spe = if Array.length tribe != 0 then avg_spe tribe else (0., 0.) in
  let tribe_pos = if Array.length tribe != 0 then avg_pos tribe else b.pos in
  let col_pos = (closer_bird b barr).pos in
  let npos = vector_in_range (b.pos +% b.spe) (0., 0.) (foi x_def, foi y_def) in
  (*if Array.length col != 0 then*)
  let nspe_dir = unit_vector (vector_mean b.spe tribe_spe !conformity_dir) in
  let repulsion = (1. /. (distance b.pos col_pos)) ** 10. in
  let repulsion_vector =  (repulsion *% unit_vector (b.pos -% col_pos)) in
  let attraction_vector = tribe_pos -% b.pos in
  let nspe_pos =  
                     (vector_mean
                        repulsion_vector
                        attraction_vector
                        !repulsion_attraction_balance
                     )
  in
  let nspe = speed *% unit_vector (vector_mean nspe_dir nspe_pos !dir_pos_balance) in
  {pos = npos; spe= nspe}
  
let random_bird_array n_birds =
  (Array.map
     (function x -> {pos = (Random.float (foi x_def), (Random.float (foi y_def)));
                     spe = speed *%
                             unit_vector ((Random.float 10.) -. 5.,
                                          (Random.float 10.) -. 5.)})
     (Array.make n_birds 0))
  
let birds_fly n_birds =
  let bird_array = ref (random_bird_array n_birds) in
  let b = ref true in
  let i = ref 0 in
  while !b do
    (*if !i <= 2 then
      print_bird_array !bird_array;
      (*Array.iter (function bi -> printf "%F %F" (angle bi.spe)) !bird_array;*)
     *)
    draw_bird_array !bird_array;
    (*let s = wait_next_event [Key_pressed; Button_down] in
    if s.keypressed then (
      if s.key = 'Q' then b := false);*)
    bird_array := Array.map (next_bird !bird_array) !bird_array;
    (*printf "\n avg speed : %F" (norm (avg_spe !bird_array));
    printf "\n avg angle : %F\n" (avg_angle !bird_array);*)
    i := !i + 1
  done;
;;
open_graph (String.concat "" [" "; (string_of_int x_def); "x"; (string_of_int y_def);]);
birds_fly 200;
exit 0
;;
