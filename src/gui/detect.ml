let get_dims img =
  ((Sdlvideo.surface_info img).Sdlvideo.w, (Sdlvideo.surface_info img).Sdlvideo.h)
 
let sdl_init () =
  begin
    Sdl.init [`EVERYTHING];
    Sdlevent.enable_events Sdlevent.all_events_mask;
  end
 
let rec wait_key () =
  let e = Sdlevent.wait_event () in
    match e with
    Sdlevent.KEYDOWN _ -> ()
      | _ -> wait_key ()


let show img dst =
  let d = Sdlvideo.display_format img in
    Sdlvideo.blit_surface d dst ();
    Sdlvideo.flip dst


let line_this dst y =
  let (w,h) = get_dims dst in
  let red = (255, 0, 0) in
  let pixel = ref (0,0,0) in
  let yTemp = ref 0 in
  let count = ref w in
    yTemp := y;
    while !count > 2 do
      count := 0;
      for x=0 to w-1 do
        pixel := Sdlvideo.get_pixel_color dst x !yTemp;
        if !pixel <= (150,150,150) then
          count := !count + 1
      done;
      yTemp := !yTemp + 1;
    done;
    if (!yTemp - y) > 5 then 
    for x=0 to w-1 do
      if Sdlvideo.get_pixel_color dst x (y-1) > (150,150,150) then
        Sdlvideo.put_pixel_color dst x (y-1) red;
	    if Sdlvideo.get_pixel_color dst x (!yTemp-1) > (150,150,150)then
     	  Sdlvideo.put_pixel_color dst x (!yTemp-1) red
    done;
    !yTemp-y

let clear_this img ymin ymax =
  let (w,h) = get_dims img in
  let pixel = ref (0,0,0) in
  let white = ref true in
  let inCar = ref 0 in
  let trans = ref false in
  for x=0 to w-1 do
    white := true;
    for y=(ymin+1) to (ymax-1) do
      pixel := Sdlvideo.get_pixel_color img x y;
      white := !white && !pixel > (150,150,150);
    done;
    if !white then
      begin
        if !inCar > 1 then
          trans := true;
        inCar := 0;
        Sdlvideo.put_pixel_color img x ymin (255,255,255);     
        Sdlvideo.put_pixel_color img x ymax (255,255,255)         
      end
    else
      inCar := !inCar + 1;
    if !trans then
      begin
        for y=ymin to ymax do
          Sdlvideo.put_pixel_color img x y (255,0,0)
        done;
        trans := false
      end;
    if !inCar = 2 then
      for y=ymin to ymax do
        Sdlvideo.put_pixel_color img (x-2) y (255,0,0);
      done;
  done;
  ()
 

let circle_this img =
  let (w,h) = get_dims img in
  let count = ref 0 in
  let tempo = ref 0 in
  let line = ref false in
  let pixel = ref (0,0,0) in
  let ymin = ref 0 in
    for y=0 to h-1 do
      count := 0;
      if not(!line) then
        begin
          for x=0 to w-1 do
            pixel := Sdlvideo.get_pixel_color img x y; 
            if !pixel <= (150,150,150) then
              count := !count + 1;
          done;
          if !count > 2 then
            begin
              tempo := line_this img y;
              line := true
            end
        end
        else
        begin
          if (!tempo <= 0) then
            line := false
          else
            tempo := !tempo - 1
        end
    done;
    line := false;
    for y=0 to h-1 do
      pixel := Sdlvideo.get_pixel_color img 0 y;
      if !pixel = (255,0,0) then
        begin
          if !line then
            begin
              line := false;
              clear_this img !ymin y;
            end
          else
            begin
              ymin := y;
              line := true;
            end
        end
    done;
    ()

(*let main () = 
    begin
      if Array.length (Sys.argv) < 2 then
      failwith "Need a pic";
      sdl_init ();
    let pic = Sdlloader.load_image Sys.argv.(1) in
    let (w,h) = get_dims pic in
    (*let surf =  Sdlvideo.create_RGB_surface_format (Rotation.rotation pic 3.) [] x y  in*)
    let surface = Sdlvideo.set_video_mode w h [`DOUBLEBUF] in
    show pic surface;
    wait_key();
    circle_this pic;
    show pic surface;
    wait_key ();
    exit 0
    end

let _ = main ()*)
