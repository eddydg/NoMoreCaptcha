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
    if Array.length (Sys.argv) < 3 then
      failwith "Need a number";
    sdl_init ();

    (*Basic instructions that show the picture*)
    let pic = Sdlloader.load_image Sys.argv.(1) in
    let (w,h) = Image_tools.get_dim pic in
    let surface = Sdlvideo.set_video_mode w h [`DOUBLEBUF] in
    

    let num = int_of_string (Sys.argv.(2)) in
    match num with
    | 1 (*Rotation*) ->
      Image_tools.print_image pic surface;
      wait_key();
      let angle = Rotation.get_angle pic in
      Printf.printf "Detected angle : %f\n" angle;
      wait_key();
      let imgrot = Rotation.rotation pic (angle) in
      Image_tools.print_image imgrot surface;
      wait_key();
      let m = Rotation.get_hough_matrix pic in
      let img2 = Image_tools.matrix2image m w h in
      Image_tools.print_image img2 surface;
      wait_key();
    | 2 (*Preprocessing*) ->
      Image_tools.print_image pic surface;
      wait_key();
      Fonctions.image2grey pic pic;
      Image_tools.print_image pic surface;
      wait_key();
      let pic2 = Sdlvideo.create_RGB_surface_format pic [] w h in
      Fonctions.noNoise_average pic pic2;
      Image_tools.print_image pic2 surface;
      wait_key();
      Fonctions.noNoise_median pic pic2;
      Image_tools.print_image pic2 surface;
      wait_key();
      Fonctions.blackAndWhite pic pic2;
      Image_tools.print_image pic2 surface;
      wait_key()
    | 3 (*character detection*) ->
      Image_tools.print_image pic surface;
      wait_key();
      Detect.circle_this pic;
      Image_tools.print_image pic surface;
      wait_key()
    | 4 (*neural network*)->
      Random.self_init();
      Neural_network.trainAndRun ()
    | _ ->
      Image_tools.print_image pic surface;
      wait_key();
      let pic2 = Rotation.rotation pic (-20.) in
      Image_tools.print_image pic2 surface;
      wait_key();
      Fonctions.image2grey pic2 pic2;
      Image_tools.print_image pic2 surface;
      wait_key();
      let (w1,h1) = Image_tools.get_dim pic2 in
      let pic3 = Sdlvideo.create_RGB_surface_format pic [] w1 h1 in
      Fonctions.noNoise_average pic2 pic3;
      Image_tools.print_image pic3 surface;
      wait_key ();
      Fonctions.blackAndWhite pic3 pic3;
      Image_tools.print_image pic3 surface;
      wait_key ();
      Detect.circle_this pic3;
      Image_tools.print_image pic3 surface;
      wait_key();

    exit 0
  end

let _ = main ()
