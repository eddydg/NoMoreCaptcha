(* Init SDL *)
let sdl_init() = 
  begin
    Sdl.init[`EVERYTHING];
    Sdlevent.enable_events Sdlevent.all_events_mask;
  end

let rec wait_key () =
  let e = Sdlevent.wait_event () in
  match e with
  Sdlevent.KEYDOWN _ -> ()
  | _ -> wait_key ()

let main () =
  begin
    if Array.length (Sys.argv) < 2 then
      failwith "Need a pic";
    sdl_init ();
    let pic = Sdlloader.load_image Sys.argv.(1) in
    let (w,h) = Image_tools.get_dim pic in
    let surface = Sdlvideo.set_video_mode w h [`DOUBLEBUF] in
    Image_tools.print_image pic surface;
    wait_key();

    (*test de matrice de convolution*)
    let pic2 = Sdlvideo.create_RGB_surface_format pic [] w h
    and m = Conv_matrix.repoussage () in
    Image_tools.apply_matrix pic pic2 m 0;
    Image_tools.print_image pic2 surface;
    (*fin du test*)
    wait_key ();
    exit 0
  end

let _ = main ()
