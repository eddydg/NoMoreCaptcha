(* Get the image dimension *)
let get_dim img =
  ((Sdlvideo.surface_info img).Sdlvideo.w,
   (Sdlvideo.surface_info img).Sdlvideo.h)


(* Print image on surface *)
let print_image img surf =
  let pic = Sdlvideo.display_format img in
  Sdlvideo.blit_surface pic surf ();
  Sdlvideo.flip surf
