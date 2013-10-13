let pi = 4. *. atan 1.

(* Convert degrees to radian *)
let deg2rad a =
  pi *. a /. 180.

(* Rotate x,y from center cx,cy by angle a *)
let rotate (x,y) (cx,cy) angle = 
  let a = deg2rad angle in
  let xf = (float_of_int(x) -. cx) *. cos a
       +. (float_of_int(y) -. cy) *. sin a
       +. cx
  and
      yf = (float_of_int(x) -. cy) *. cos a
       +. (cx -. (float_of_int(x))) *. sin a
       +. cy 
  in
  (int_of_float(xf), int_of_float(yf));;

(* Get the max dimensions of image after rotation *)
let dims_max h w angle =
  let a = deg2rad angle  in
  let hf = h *. cos a +. w *. sin a and
      wf = h *. sin a +. w *. cos a in
  (hf, wf)

