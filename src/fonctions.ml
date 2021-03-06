(*GREY FILTER*)

let level ((a:int),(b:int),(c:int)) =
        (0.3*.(float_of_int a) +. 0.59*.(float_of_int b) +. 0.11*.(float_of_int c))

let color2grey (a,b,c) =
        let x = level (a, b, c) in
        (int_of_float x,int_of_float x,int_of_float x)

let image2grey src =
  let dst = src in
  let x,y,z = Sdlvideo.surface_dims src in
    for i = 0 to (x-1) do
      for j = 0 to (y-1) do
        Sdlvideo.put_pixel_color dst i j (color2grey (Sdlvideo.get_pixel_color src i j));
      done;
    done;
    dst

let g_color src i j = Sdlvideo.get_pixel_color src i j

(*BLACK AND WHITE FILTER*)
let average_color src =
	let r =ref 0. in
	let x,y,z = Sdlvideo.surface_dims src in
	for i = 0 to (x-1) do
	  for j = 0  to (y-1) do
	    let a,b,c = (Sdlvideo.get_pixel_color src i j) in
		let res = (float_of_int(a+b+c))/.3. in
		r := (!r +. res); 
	  done;
	done;
	(!r/.(float_of_int(x*y)))

let blackAndWhite src =
	let dst = src in
        let x,y,z = Sdlvideo.surface_dims src in
	let average = average_color src in
	Printf.printf "Moyenne : %f\n" (average);
		for i = 0 to (x-1) do
                        for j = 0 to (y-1) do
                        let a = level (Sdlvideo.get_pixel_color src i j) in
                                if (a < (average)) then
                                        Sdlvideo.put_pixel_color dst i j (0, 0, 0)
                                else
                                        Sdlvideo.put_pixel_color dst i j (255, 255, 255)
                        done;
                done;
	dst

(*NOISE REDUCTION*)
let get_list src i j =
  let (x,y,_) = Sdlvideo.surface_dims src in
  let tab = Array.make 8 (-1,-1,-1) in
	if((i-1 >= 0) && j >= 0 && i-1 < x-1 && j < y-1) then tab.(0) <- g_color src (i-1) j;
	if(i >= 0 && (j+1 >= 0) && i < x-1 && j-1 < y-1) then tab.(1) <- g_color src i (j-1);
	if(i >= 0 && (j+1 >= 0) && i < x-1 && j+1 < y-1) then tab.(2) <- g_color src i (j+1);
	if((i+1 >= 0) && j >= 0 && i+1 < x-1 && j < y-1) then tab.(3) <- g_color src (i+1) j;
  tab

let average_px tab (e,f,g)= 
  let a,b,c = (ref 0, ref 0, ref 0) in
  let nb_px = ref 0 in
    for i = 0 to ((Array.length tab) - 1) do
	if (not(tab.(i) = (-1,-1,-1))) then
	begin
	  let (x,y,z) = tab.(i) in
	  a := !a + x;
	  b := !b + y;
	  c := !c + z;
	  nb_px := !nb_px + 1;
	end
    done;
  if(not(!nb_px = 0)) then
    let u,v,w = ((!a / !nb_px), (!b / !nb_px), (!c / !nb_px)) in
    ((u+e)/2,(v+f)/2,(w+g)/2)
  else
    (0,0,0)

let compare_triplet a b =
	let level_a = level a in
	let level_b = level b in
	match (level_a, level_b) with
	(a,b) when a=b -> 0
	|(a,b) when a>b -> 1
	|_ -> -1
	    

let median_px tab = 
  Array.fast_sort compare_triplet tab;
  let nb_px = ref 0 in
    for i = 0 to (Array.length tab) -1 do
      if not(tab.(i) = (-1, -1, -1)) then
      begin
	nb_px := !nb_px + 1;
      end
    done;
  tab.((!nb_px / 2) + (8 - !nb_px))	

let noNoise_median src =
  let dst = src in
  let (w,h) = Image_tools.get_dim src in
    for i = 0 to (w-1) do
      for j = 0 to (h-1) do
        Sdlvideo.put_pixel_color dst i j (median_px (get_list src i j));
      done;
    done;
  dst


let noNoise_average src =
  let dst = src in
  let (w,h) = Image_tools.get_dim src in
    for i = 1 to (w-1) do
      for j = 1 to (h-1) do
	Sdlvideo.put_pixel_color dst i j (average_px (get_list src i j) (Sdlvideo.get_pixel_color src i j));
      done;
    done;
 dst

(* TEST SEUILLAGE INTELLIGENT*)
let seuil src =
  let nb_px = ref 0 in
  let accu = ref 0 in
  let (w,h) = Image_tools.get_dim src in
  for i = 0 to (w-1) do
    for j = 0 to (h-1) do
      if (level (g_color src i j) < 220.) then
	begin
	accu := !accu + int_of_float(level(g_color src i j));
	nb_px := !nb_px + 1;
	end
    done;
  done;
  Printf.printf "Nb_px : %d\n" !nb_px;
  (!accu / !nb_px)

let blackAndWhite2 ?vraiseuil src =
	let dst = src in
	let seuil_img = match vraiseuil with
	  Some x -> x
	  |None -> float_of_int(seuil src)
	in
        let x,y,z = Sdlvideo.surface_dims src in
       	Printf.printf "Seuil : %f\n" (seuil_img +. 5.);
                for i = 0 to (x-1) do
                        for j = 0 to (y-1) do
                        let a = level (Sdlvideo.get_pixel_color src i j) in
                                if (a > seuil_img) then
                                        Sdlvideo.put_pixel_color dst i j (255,255,255)
                                else
                                        Sdlvideo.put_pixel_color dst i j (0, 0, 0)
                        done;
                done;
  dst

