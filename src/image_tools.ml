let pi = 4. *. atan 1.

(* Get the image dimension *)
let get_dim img =
  ((Sdlvideo.surface_info img).Sdlvideo.w,
   (Sdlvideo.surface_info img).Sdlvideo.h)


(* Print image on surface *)
let print_image img surf =
  let pic = Sdlvideo.display_format img in
  Sdlvideo.blit_surface pic surf ();
  Sdlvideo.flip surf

(* Convert image to matrix  *)
let image2matrix image =
  let (w, h) = get_dim image in
  let matrix = Array.make_matrix w h (255,255,255) in
  for y = 0 to h-1 do
    for x = 0 to w-1 do
      matrix.(x).(y) <- Sdlvideo.get_pixel_color image x y
    done;
  done;
  matrix;;

(* Convert degrees to radian *)
let deg2rad a =
  pi * a / 180

(* Rotate x,y from center cx,cy by angle a *)
let rotate (x,y) (cx,cy) angle = 
  let a = deg2rad angle in
  let X = (float_of_int(x) -. cx) *. cos a
       +. (float_of_int(y) -. cy) *. sin a
       +. cx
  and
      Y = (float_of_int(x) -. cy) */ cos a
       +. (cx -. (float_of_int(x))) *. sin a
       +. cy 
  in
  (int_of_float(X), int_of_float(Y));;
