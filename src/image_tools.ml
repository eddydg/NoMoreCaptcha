(* Get the image dimension *)
let get_dim img =
  ((Sdlvideo.surface_info img).Sdlvideo.w,
   (Sdlvideo.surface_info img).Sdlvideo.h);;

(* Print image on surface *)
let print_image img surf =
  let pic = Sdlvideo.display_format img in
  Sdlvideo.blit_surface pic surf ();
  Sdlvideo.flip surf;;

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

(*determine if the point(x,y) is in the picture 'img' or not*)
let is_in_bound img x y =
  let (w,h) = get_dim img in
  x > 0 && y > 0 && x < w-1 && y < h-1;;

(*applique une matrice à un pixel de l'image*)
let apply_matrix_px img_src img_dst x y matrix bias =
  let r = ref 0 and g = ref 0 and b = ref 0
  and posX = ref 0 and posY = ref 0 and factor = ref 1
  and dimXmatrix = Array.length matrix and dimYmatrix = Array.length matrix.(0) in
  for i = 0 to dimXmatrix-1 do
    for j = 0 to dimYmatrix-1 do
      factor := !factor + matrix.(i).(j);
      posX := x - dimXmatrix / 2 + i;
      posY := y - dimYmatrix / 2 + j;
      if is_in_bound img_src !posX !posY then begin
        let (cr,cg,cb) = Sdlvideo.get_pixel_color img_src !posX !posY in
        r := !r + cr * matrix.(i).(j);
        g := !g + cg * matrix.(i).(j);
        b := !b + cb * matrix.(i).(j); end
    done; 
  done;
  r := !r / !factor + bias;
  g := !g / !factor + bias;
  b := !b / !factor + bias;
  if !r > 255 then r := 255 else if !r < 0 then r := 0;
  if !g > 255 then g := 255 else if !g < 0 then g := 0;
  if !b > 255 then b := 255 else if !b < 0 then b := 0;
  Sdlvideo.put_pixel_color img_dst x y (!r,!g,!b);;

(*applique une matrice de convolution a toute une image*)
let apply_matrix src dst matrix bias =
  let (w,h) = get_dim src in
  for i = 0 to w-1 do
    for j = 0 to h-1 do
      apply_matrix_px src dst i j matrix bias;
    done;
  done;;