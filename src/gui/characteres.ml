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
	Sdlvideo.unset_alpha image;
   Sdlvideo.blit_surface ~src_rect:rectangle ~src:image ~dst:surf ();
	surf
  

let cross_word img xmin xmax ymin ymax =
		let red = (255,0,0) in
		let xmin2 = ref 0 in
		let xmax2 = ref 0 in
		let list_of_char = ref [] in
		let charac = ref false in
		let count = ref 0 in
		for x = xmin to xmax do
			if not(!charac) && (Sdlvideo.get_pixel_color img x (ymin+1)) = red then
				begin
					charac := true;
					xmin2 := x;
				end;
			if !charac then
				begin
					count := 0;
					for y=(ymin+1) to (ymax-1) do
						if (Sdlvideo.get_pixel_color img (x+1) y) = red then
							count := !count + 1
					done;
					if !count >= 3 then
						begin
							xmax2 := (x+1);
							list_of_char := ((make_char img (!xmin2+1) (!xmax2) (ymin+1) (ymax)) :: !list_of_char);
							charac := false
						end
				end
		done;
		!list_of_char

let cross_line img ymin ymax =
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
							xmax := x;
							list_of_word := ((cross_word img !xmin (!xmax) ymin (ymax)) :: !list_of_word);
							word := false
						end
				end
		done;
		!list_of_word

let cross_img img = 
	let line = ref false in
	let red = (255,0,0) in
	let (w,h) = get_dims img in
	let list_of_line = ref [] in
	let ymax = ref 0 in
	let ymin = ref 0 in
	let white = ref true in
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
						list_of_line := ((cross_line img !ymin !ymax) :: !list_of_line);
						line := false
					end
			end
	done;
	!list_of_line 



let surf_test pic = 	
		let list1 = cross_img pic in
		let list2 = ref (List.hd list1) in
		let list3 = ref (List.hd !list2) in
		for i = 0 to List.length list1 -1 do
			for j = 0 to List.length (List.nth list1 i) -1 do
				for k = 0 to List.length (List.nth (List.nth list1 i) j) -1 do
					let (w2,h2) = get_dims (List.nth (List.nth (List.nth list1 i) j) k) in
					let surface2 = Sdlvideo.set_video_mode w2 h2 [`DOUBLEBUF] in
					show (List.nth (List.nth (List.nth list1 i) j) k) surface2;
					wait_key ();
				done;
			done;
		done


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
    surf_test pic;
    wait_key ();
    exit 0
    end

let _= main () 