(*FILTRE GRIS*)

let level ((a:int),(b:int),(c:int)) =
        (0.3*.(float_of_int a) +. 0.59*.(float_of_int b) +. 0.11*.(float_of_int c))

let color2grey (a,b,c) =
        let x = level (a, b, c) in
        (int_of_float x,int_of_float x,int_of_float x)

let image2grey src dst =
  let x,y,z = Sdlvideo.surface_dims src in
    for i = 0 to (x-1) do
      for j = 0 to (y-1) do
        Sdlvideo.put_pixel_color dst i j (color2grey (Sdlvideo.get_pixel_color src i j));
      done;
    done

let g_color src i j = Sdlvideo.get_pixel_color src i j

(*FILTRE NOIR ET BLANC*)
let moyenne_couleur src =
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

let blackAndWhite src dst =
        let x,y,z = Sdlvideo.surface_dims src in
	let moyenne = moyenne_couleur src in
               (* Printf.printf "%d\n" (int_of_float moyenne);*)
		for i = 0 to (x-1) do
                        for j = 0 to (y-1) do
                        let a = level (Sdlvideo.get_pixel_color src i j) in
                                if (a < (moyenne -. 40.)) then
                                        Sdlvideo.put_pixel_color dst i j (0, 0, 0)
                                else
                                        Sdlvideo.put_pixel_color dst i j (255, 255, 255)
                        done;
                done

(*REDUCTION BRUIT*)
let get_list src i j =
  let (x,y) = Image_tools.get_dim src
  and n = ref 0 in
  let tab = Array.make 8 (-1,-1,-1) in
  for a = i-1 to i+1 do
    for b = j-1 to j+1 do
      if (a >= 0 && a < (x-1) && b >= 0 && b < (y-1) && not(a = i) && not(b = j)) then
      begin  
	tab.(!n) <- g_color src a b;
        n := !n+1;
      end
    done;
  done;
  tab

let average_px tab = 
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
  let u,v,w = ((!a / !nb_px), (!b / !nb_px), (!c / !nb_px)) in
  (u,v,w)

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

let noNoise_median src dst =
  let (w,h) = Image_tools.get_dim src in
    for i = 1 to (w-1) do
      for j = 1 to (h-1) do
        Sdlvideo.put_pixel_color dst i j (median_px (get_list src i j));
      done;
    done


let noNoise_average src dst =
  let (w,h) = Image_tools.get_dim src in
    for i = 1 to (w-1) do
      for j = 1 to (h-1) do
	Sdlvideo.put_pixel_color dst i j (average_px (get_list src i j));
      done;
    done
