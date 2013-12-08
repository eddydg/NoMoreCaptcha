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

let create_surface w h = 
	Sdlvideo.create_RGB_surface 
		[`SWSURFACE] 
		~w:w 
		~h:h 
		~bpp:32 	
		~rmask:Int32.zero 
		~gmask:Int32.zero 
		~bmask:Int32.zero 
		~amask:Int32.zero

let make_char image xmin xmax ymin ymax = 
	let surf = create_surface (xmax-xmin) (ymax-ymin) in
	let rectangle = Sdlvideo.rect xmin ymin (xmax-xmin) (ymax-ymin) in
   Sdlvideo.blit_surface ~src_rect:rectangle ~src:image ~dst:surf ();
	surf
  

(*let parcours_word img xmin xmax ymin ymax =
		let red = (255,0,0) in
		let xmin2 = ref 0 in
		let xmax2 = ref 0 in
		let ymin2 = ref 0 in
		let ymax2 = ref 0 in
		let list_of_char = ref [] in
		let charac = ref false in
		let green = (0,255,0) in
		let count = ref 0 in
		for x = (xmin+1) to xmax do
			for y = ymin to ymax do
				if not(!charac) && (Sdlvideo.get_pixel_color img (x-1) y) = green then
					begin
						charac := true;
						xmin2 := x;
						ymin2 := y
					end
			done;
			if !charac then
				begin
					for y=(ymin+1) to (ymax-1) do
						if (Sdlvideo.get_pixel_color img (x+1) y) = red then
							count := !count + 1
					done;
					if !count >= 3 then 
						Sdlvideo.put_pixel_color img x ymin green;
					if (Sdlvideo.get_pixel_color img x ymin) = green then
						begin
							xmax2 := x;
							ymax2 := ymax;
							list_of_char := ((make_char img !xmin2 !xmax2 !ymin2 !ymax2) :: !list_of_char);
							charac := false
						end
				end
		done;
		!list_of_char

let parcours_line img ymin ymax =
		let (w,h) = get_dims img in
		let red = (255,0,0) in
		let white = ref true in 
		let xmin = ref 0 in
		let xmax = ref 0 in
		let word = ref false in
		let next_red = ref true in
		let list_of_word = ref [] in
		for x = 0 to w-1 do
			white := true;
				if not(!word) && (Sdlvideo.get_pixel_color img x ymin) = red then
					begin
						word := true;
						xmin := x
					end;
			if !word then
				begin
					for y =ymin to ymax do
						white := !white && (Sdlvideo.get_pixel_color img x y) > (150,150,150);
					done;
					for x2 = x to (x+1) do
						next_red := (Sdlvideo.get_pixel_color img x2 ymin) = red;
					done;
					if !white && not(!next_red) then
						begin
							Sdlvideo.put_pixel_color img x ymin (0,255,0);
							xmax := x;
							list_of_word := ((parcours_word img !xmin !xmax ymin ymax) :: !list_of_word);
							word := false
						end
				end
		done;
		!list_of_word

let parcours_img img = 
	let line = ref false in
	let red = (255,0,0) in
	let (w,h) = get_dims img in
	let list_of_line = ref [] in
	let ymax = ref 0 in
	let ymin = ref 0 in
	let white = ref true in
	let count = ref 0 in 
	for y = 0 to h-1 do
		white := true;
		for x = 0 to w-1 do
			if not(!line) && (Sdlvideo.get_pixel_color img x y) = red then
				begin
					line := true;
					Sdlvideo.put_pixel_color img x y (0,255,0);
					ymin := y
				end
		done;
		if !line then
			begin
				for x = 0 to w-1 do
					white := !white && (Sdlvideo.get_pixel_color img x y) > (150,150,150);
				done;
				if !white then
					begin
						ymax := y - 1;
						list_of_line := ((parcours_line img !ymin !ymax) :: !list_of_line);
						line := false
					end
			end
	done;
	!list_of_line *)
let line_this dst y =
  let (w,h) = get_dims dst in
  let red = (255, 0, 0) in
  let pixel = ref (0,0,0) in
  let yTemp = ref 0 in
  let count = ref w in
    yTemp := y;
    while !count > 5 do
      count := 0;
      for x=0 to w-1 do
        pixel := Sdlvideo.get_pixel_color dst x !yTemp;
        if !pixel <= (150,150,150) then
          count := !count + 1
      done;
      yTemp := !yTemp + 1;
    done;
    if (!yTemp - y) > 5 then 
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
          if !count > 5 then
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

let rec long = function [] -> 0
						| e :: l -> 1 + long l

let surf_test pic = 	
		let list1 = parcours_img pic in
		let list2 = List.nth list1 6 in
		let list3 = List.hd list2 in
		let (x,y,z) = ((long list1),(long list2),(long list3)) in
		Printf.printf "%d\n" x;
		Printf.printf "%d\n" y;
		Printf.printf "%d\n" z;
		List.hd list3


let main () = 
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
	Detect.circle_this pic;
	show pic surface;
    wait_key ();
    let (w2,h2) = get_dims (surf_test pic)in 
    let surface2 = Sdlvideo.set_video_mode w2 h2 [`DOUBLEBUF] in
    show (surf_test pic) surface2;
    wait_key ();
    exit 0
    end

let _= main () 