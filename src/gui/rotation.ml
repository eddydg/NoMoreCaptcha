let pi = 4. *. atan 1.
let abs_f x = if (x < 0.) then (-1. *. x) else x

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
  (xf, yf)

(* Get the dimensions of image after rotation *)
let dims_rot w h angle =
  let a = deg2rad (abs_f angle) in
  let hf = (float_of_int h) *. cos a +. (float_of_int w) *. sin a and
      wf = (float_of_int h) *. sin a +. (float_of_int w) *. cos a in
  ((int_of_float (ceil wf)), (int_of_float (ceil hf)))

(* Apply the rotation to each pixel  of image *)
let rotation image angle =
  let angle = -1. *. angle in
  let (w1, h1) = Image_tools.get_dim image in
  let (w, h) = dims_rot w1 h1 angle in 
  let new_surface = Sdlvideo.set_video_mode w h [`DOUBLEBUF] in
  begin

    for y = 0 to h-1 do
      for x = 0 to w-1 do
        Sdlvideo.put_pixel_color new_surface x y (255,255,255);
      done;
    done;

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
  end;
  new_surface

let get_hough_matrix image = 
  let (w, h) = Image_tools.get_dim image in
  let detected_lines = Array.make_matrix 180 (w+h) 0 in
  for y = 0 to h-1 do
    for x = 0 to w-1 do
      let color = Sdlvideo.get_pixel_color image x y in

      if color <> (255,255,255) then
        for theta = 0 to 179 do
          let theta_rad = deg2rad (float_of_int theta) -. (deg2rad 90.) in
          let rho = (float_of_int x)*.(cos theta_rad) +.
                    (float_of_int y)*.(sin theta_rad) in
          let rho = (int_of_float (abs_f rho)) in
          detected_lines.(theta).(rho) <- detected_lines.(theta).(rho) + 1;
        done;
        
    done;
  done;
  detected_lines


let get_angle image =
  let (w, h) = Image_tools.get_dim image in
  let hough_matrix = get_hough_matrix image in
  let max = ref 0 in
  let angle = ref 0 in
  for theta = 0 to 179 do
    for rho = 0 to (w+h-1) do
      if (hough_matrix.(theta).(rho) > !max) then
      begin
        max := hough_matrix.(theta).(rho);
        angle := theta;
        Printf.printf "%d " !angle;
      end
    done;
  done;
  Printf.printf "theta = %d\n" (!angle);
  float_of_int (!angle)
