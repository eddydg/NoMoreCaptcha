let pi = 4. *. atan 1.

(* Convert degrees to radian *)
let deg2rad a =
  pi *. a /. 180.

(* Return the (x,y) of the image center  *)
let image_center w h =
  ((float_of_int w) /. 2., (float_of_int h) /. 2.)

(* Rotate x,y from center cx,cy by angle a *)
let rotate (x,y) (cx,cy) angle = 
  let a = deg2rad angle in
  let xf = (float_of_int(x) -. cx) *. cos a
       -. (float_of_int(y) -. cy) *. sin a
       +. cx
  and
      yf = (float_of_int(x) -. cx) *. sin a
       +. ((float_of_int(y)) -. cy) *. cos a
       +. cy 
  in
  (xf, yf);;

(* Get the dimensions of image after rotation *)
let dims_rot w h angle =
  let a = deg2rad angle in
  let hf = (float_of_int h) *. cos a +. (float_of_int w) *. sin a and
      wf = (float_of_int h) *. sin a +. (float_of_int w) *. cos a in
  ((int_of_float (ceil wf)), (int_of_float (ceil hf)))

(* Apply the rotation to each pixel  of image *)
let rotation image angle =
  let (w1, h1) = Image_tools.get_dim image in
  let (w, h) = dims_rot w1 h1 angle in 
  let new_surface = Sdlvideo.set_video_mode w h [`DOUBLEBUF] in

  for y = 0 to h1-1 do
    for x = 0 to w1-1 do
      let pixel_color = Sdlvideo.get_pixel_color image x y in
      let (x_rot, y_rot) = rotate (x, y) (image_center w1 h1) angle in 
      let (x_rot, y_rot) = (x_rot +. (float_of_int (w-w1))/.2.,
                            y_rot +. (float_of_int (h-h1))/.2.) in (* Shift to recenter *)
      if (x_rot >= 0. && x_rot < (float_of_int w) && y_rot >= 0. && y_rot < (float_of_int h)) then
	       Sdlvideo.put_pixel_color new_surface (int_of_float x_rot)
                                              (int_of_float y_rot)
                                              pixel_color;
    done;
  done;
  new_surface

(*

  let detect_angle image = 
    let (w, h) = get_dim image in
    let detected_lines = Array.make_matrix 100 3 0 in
    let i = 0 in
    for y = 0 to h-1 do
      for x = 0 to w-1 doux
        let color = Sdlvideo.get_pixel_color image x y in
        if color = (0,0,0) then
          let theta = int_of_float (deg2rad (pi/2)) in
            while theta < int_of_float (deg2rad (2*pi)) do
              detected_lines.(i).(0) = detected_lines.(i).(0) + 1
              detected_lines.(i).(1) = theta
              detected_lines.(i).(2) = ro
              incr i;
            done;
            for 
          *)
