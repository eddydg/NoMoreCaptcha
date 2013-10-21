(*FILTRE GRIS*)

let level ((a:int),(b:int),(c:int)) =
        (0.3*.(float_of_int a) +. 0.59*.(float_of_int b) +. 0.11*.(float_of_int c))

let color2grey (a,b,c) =
        let x = level (a, b, c) in
        (int_of_float x,int_of_float x,int_of_float x)

let image2grey src dst =
  let x,y,z = Sdlvideo.surface_dims src in
    for i = 0 to x do
      for j = 0 to y do
        Sdlvideo.put_pixel_color dst i j (color2grey (Sdlvideo.get_pixel_color src i j));
      done;
    done

(*FILTRE BRUIT*)
let rec sort = function
  | [] -> []
  | x :: l -> insert x (sort l)

and insert elem = function
  | [] -> [elem]
  | x :: l ->
      if elem < x then elem :: x :: l else x :: insert elem l;;

let median l = let l1 = sort l in
        let rec nth l i = match (l,i) with
                ([],_) -> failwith "Liste trop petite !"
                |(e::l,0) -> e
                |(e::l,a) -> nth l (a-1) in
        nth l1 4

let g_color src i j = Sdlvideo.get_pixel_color src i j

let sansBruit src dst =
  let x,y,_ = Sdlvideo.surface_dims src in
    for i = 1 to (x-1) do
      for j = 1 to (y-1) do
        Sdlvideo.put_pixel_color dst i j (median [g_color src (i-1) j; g_color src (i-1) (j-1); g_color src (i-1) (j+1); g_color src i (j-1); g_color src i (j+1); g_color src (i+1) j; g_color src (i+1) (j-1); g_color src (i+1) (j+1)])
      done;
    done

(*FILTRE NOIR ET BLANC*)
let moyenne_couleur src =
	let r =ref 0. in
	let x,y,z = Sdlvideo.surface_dims src in
	for i = 0 to x do
	  for j = 0  to y do
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
		for i = 0 to x do
                        for j = 0 to y do
                        let a = level (Sdlvideo.get_pixel_color src i j) in
                                if (a < (moyenne -.0.)) then
                                        Sdlvideo.put_pixel_color dst i j (0, 0, 0)
                                else
                                        Sdlvideo.put_pixel_color dst i j (255, 255, 255)
                        done;
                done

let get_list src i j =
  let (x,y) = Image_tools.get_dim src
  and n = ref 0 in
  let tab = Array.make 8 (-1,-1,-1) in
  for a = i-1 to i+1 do
    for b = j-1 to j+1 do
      if (a >= 0 && a < x && b >= 0 && b < y && a != i && b != j) then
        tab.(!n) <- g_color src a b;
        n := !n +1;
    done;
  done;
  tab
