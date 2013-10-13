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
