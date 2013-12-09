let round x = if x -. 0.5 < float_of_int (int_of_float x) then int_of_float (floor x) else int_of_float (ceil x)

let rec wait_key () =
  let e = Sdlevent.wait_event () in
  match e with
  Sdlevent.KEYDOWN _ -> ()
  | _ -> wait_key ()


let normalize2 src c r =
begin
	let w,h = Tools.get_dim (src) in
    let surface = Sdlvideo.set_video_mode w h [`DOUBLEBUF] in
    Tools.print_image (src) surface;
    wait_key ();

	let a = Array.init ((c)*(r))
	(fun i ->
		let r,g,b = Sdlvideo.get_pixel_color src ((i mod c)*(c/w)) ((i / c)*(r/h)) in
			(if Tools.level (r,g,b) > 127. then 0. else 1.)) in
	for x = 0 to c do
		for y = 0 to r do
			Printf.printf "%.0f " a.(x*c+y);
		done;
		Printf.printf "\n";
	done;
	a
end

let normalize src columns rows =
	(* fill with blanck before*)
	Sdlvideo.unset_alpha src;
    let croped = Tools.auto_crop (Sdlvideo.unset_alpha src; src) in
    let dst = Tools.fill_with_blank (Sdlvideo.unset_alpha croped; croped) in
    Sdlvideo.unset_alpha dst;

    (*let w,h = Tools.get_dim (dst) in
    let surface = Sdlvideo.set_video_mode w h [`DOUBLEBUF] in
    Tools.print_image (dst) surface;
    wait_key ();*)
	
	let w,h = Tools.get_dim dst in
	let x = ref 0. in
	let y = ref 0. in
	let xInterval =(float_of_int (w-1)) /. (float_of_int columns) in
	let yInterval =(float_of_int (h-1)) /. (float_of_int rows) in
	let res = Array.make ((columns+1) * (rows+1)) 8. in
	let taille = Array.length res in
	let count = ref 0 in
	y := 0.;
	x := 0.;
	while !count < taille do
		if !x > float_of_int (w-1) then
		begin
			x := 0.;
			y := !y +. yInterval;
		end;
		let (r,g,b) = (Sdlvideo.get_pixel_color dst (round !x) (round !y)) in
		res.(!count) <- (if Tools.level (r,g,b) > 127. then 0. else 1.); (* 1 = black ; 0 = white *)
		count := !count +1;
		x := !x +. xInterval;
	done;
	Printf.printf"count : %d\n%!" !count;
	res

let readLine f =
begin
	try
		input_line f;
	with
	| End_of_file -> "";
end

let posMax tab =
begin
	let pos = ref 0 in
	for i = 0 to Array.length tab -1 do
		if tab.(!pos) < tab.(i) then
			pos := i;
	done;
	!pos;
end

class network =
object (this)
	val mutable hLayers = Array.make_matrix 1 1 (new Neuron.neuron);
	val mutable oLayers = Array.init 1 (fun _ -> new Neuron.neuron);
	val mutable learnRate = 0.5;
	val mutable nbInputs = 0;

	method layer_length n =
	begin
		if n < 0 || n > Array.length hLayers +1 then
		begin
			failwith "network.ml (layer_length) : invalid arg";
		end
		else
		begin
			if n = Array.length hLayers then
			begin
				Array.length oLayers;
			end
			else
			begin
				Array.length hLayers.(n);
			end
		end
	end

	method s_neuron layerIndex neuronIndex tabOfW bias =
	begin
		if layerIndex = Array.length hLayers then (* on out layer *)
		begin
			oLayers.(neuronIndex)#s_weights tabOfW;
			oLayers.(neuronIndex)#s_bias bias;
		end
		else (* on a hidden layer *)
		begin
			hLayers.(layerIndex).(neuronIndex)#s_weights tabOfW;
			hLayers.(layerIndex).(neuronIndex)#s_bias bias;
		end
	end

	method make nbIn liste rate =
	begin
		if Array.length liste < 2 then failwith "network.ml : (network.make) invalid network, you need at least 2 layers in your network";

		Random.self_init ();
		learnRate <- rate;
		nbInputs <- nbIn;
		hLayers <- Array.make_matrix (Array.length liste - 1) 1 (new Neuron.neuron);
		oLayers <- Array.init (liste.(Array.length liste -1)) (fun _ -> new Neuron.neuron);

		for i = 0 to Array.length liste -2 do
			hLayers.(i) <- Array.init (liste.(i)) (fun _ -> new Neuron.neuron);
			for j = 0 to Array.length hLayers.(i) -1 do
				hLayers.(i).(j)#init (if i = 0 then nbIn else liste.(i-1));
			done;
		done;

		oLayers <- Array.init (liste.(Array.length liste -1)) (fun _ -> new Neuron.neuron);
	    for i = 0 to Array.length oLayers -1 do
	        oLayers.(i)#init (liste.(Array.length liste - 2));
	    done;
	end

	method train tabIN result =
	begin
		(*set inputs for each neuron*)
	    for i = 0 to Array.length hLayers -1 do
	        for j = 0 to Array.length hLayers.(i) -1 do
	            if i = 0 then begin (*First Layer*)
	                hLayers.(i).(j)#s_inputs tabIN;
	            end
	            else begin (*Other layers*)
	                hLayers.(i).(j)#s_inputs (Neuron.out_of_layer hLayers.(i-1));end
	        done;
	    done;

	    (*set inputs for the outputs neurons*)
	    for i = 0 to Array.length oLayers -1 do
	        oLayers.(i)#s_inputs (Neuron.out_of_layer hLayers.(Array.length hLayers -1));
	    done;

	    (* compute finals outputs*)
	    let finalOut = Neuron.out_of_layer oLayers in
	    (*calculate the error*)
	    for i = 0 to Array.length oLayers -1 do
	        oLayers.(i)#s_adjustment (finalOut.(i) *. (1. -. finalOut.(i)) *. (result.(i) -. finalOut.(i)));
	    done;
	    for i = 0 to Array.length hLayers -1 do
	        for j = 0 to Array.length hLayers.(i) -1 do
	            let tmp = ref 0. in
	            let couche = if i = Array.length hLayers -1 then oLayers else hLayers.(i+1) in (* couche suivante de la couche actuelle *)
	            for k = 0 to Array.length couche -1 do
	                tmp := !tmp +. (couche.(k)#g_adjustment ()) *. (couche.(k)#g_weight j);
	            done;
	            let tmpout = hLayers.(i).(j)#g_output () in
	            hLayers.(i).(j)#s_adjustment (tmpout *. (1. -. tmpout) *. !tmp);
	        done;
	        (*let tmpOut = hNeurons.(i)#g_output() in
	        hNeurons.(i)#s_adjustment (tmpOut *. (1. -. tmpOut) *. (oNeurons.(0)#g_adjustment()) *. (oNeurons.(0)#g_weight i));*)
	    done;

	    (*adjust weight*)
	    (*for neurons*)
	    for i = 0 to Array.length hLayers -1 do
	        for j = 0 to Array.length hLayers.(i) -1 do
	            hLayers.(i).(j)#adjust_weight learnRate;
	        done;
	    done;
	    (*for output*)
	    for i = 0 to Array.length oLayers -1 do
	        oLayers.(i)#adjust_weight learnRate;
	    done;
	end

	method run tabIN =
	begin
	    for i = 0 to Array.length hLayers -1 do
	        for j = 0 to Array.length hLayers.(i) -1 do
	            if i = 0 then begin (*First Layer*)
	                hLayers.(i).(j)#s_inputs tabIN;
	            end
	            else begin (*Other layers*)
	                hLayers.(i).(j)#s_inputs (Neuron.out_of_layer hLayers.(i-1));end
	        done;
	    done;

	    (*set inputs for the outputs neurons*)
	    for i = 0 to Array.length oLayers -1 do
	        oLayers.(i)#s_inputs (Neuron.out_of_layer hLayers.(Array.length hLayers -1));
	    done;

	    (* compute finals outputs*)
	    let finalOut = Neuron.out_of_layer oLayers in
	    
	    (*Show the result*)
	    (*Printf.printf "result : ";
	    for i = 0 to Array.length finalOut -1 do
	    	Printf.printf "%f\t" finalOut.(i);
	    done;
	    Printf.printf "\n";*)
		(*finalOut;*)
		(posMax finalOut, finalOut);
	end

	method mean_square_error examples =
	begin
		let tmp = ref 0. in
		let res = Array.make (Array.length oLayers) 0. in
		for i = 0 to Array.length examples -1 do
			let ex,wOut = examples.(i) in
			let _,fOut = this#run ex in
			for j = 0 to Array.length res -1 do
				res.(j) <- res.(j) +. (fOut.(j) -. wOut.(j))*.(fOut.(j) -. wOut.(j));
			done;
		done;
		for i = 0 to Array.length res -1 do
			tmp := !tmp +. res.(i)/.2.;
		done;
		!tmp;
	end

	method save filename =
	begin
		let file = open_out filename in
		Printf.fprintf file "%d\n" (Array.length hLayers + 1); (* number of layers *)
		Printf.fprintf file "%d\n" nbInputs; (* number of inputs *)
		for i = 0 to Array.length hLayers -1  do
			Printf.fprintf file "%d\n" (Array.length hLayers.(i)); (* neurons in hLayers ith *)
		done;
		Printf.fprintf file "%d\n" (Array.length oLayers); (* neuron in oLayers *)
		Printf.fprintf file "%.2f\n" learnRate; (* learnRate *)

		(* weight & bias of hiddens layers *)
		for i = 0 to Array.length hLayers -1 do
			for j = 0 to Array.length hLayers.(i) -1 do
				let w = hLayers.(i).(j)#g_all_weights () in
				for k = 0 to Array.length w -1 do
					Printf.fprintf file "%.40f\n" w.(k);
				done;
				Printf.fprintf file "%.40f\n" (hLayers.(i).(j)#g_bias ());
			done;
		done;
		(* weight & bias of output layer *)
		for i = 0 to Array.length oLayers -1 do
			let w = oLayers.(i)#g_all_weights () in
				for k = 0 to Array.length w -1 do
					Printf.fprintf file "%.40f\n" w.(k);
				done;
				Printf.fprintf file "%.40f\n" (oLayers.(i)#g_bias ());
		done;

		close_out file;
		Printf.printf "Saved to %s\n" filename;
	end
end

let load filename =
begin
	let file = open_in filename in
	let network = new network in

	let nbOfLayers = int_of_string (readLine file) in
	let nbOfInputs = int_of_string (readLine file) in
	let tabNetwork = Array.make nbOfLayers 0 in
	for i = 0 to nbOfLayers -1 do
		tabNetwork.(i) <- int_of_string (readLine file);
	done;
	let rate = float_of_string (readLine file) in
	network#make nbOfInputs tabNetwork rate;

	for i = 0 to nbOfLayers -2 do
		for j = 0 to (network#layer_length i) -1 do (* for each neuron of each layer *)
			let w = Array.make (if i = 0 then nbOfInputs else (network#layer_length (i-1)))  0. in
			for k = 0 to Array.length w -1 do
				w.(k) <- float_of_string (readLine file);
			done;
			let bias = float_of_string (readLine file) in
			network#s_neuron i j w bias;
		done;
	done;

	for i = 0 to network#layer_length (nbOfLayers -1) -1 do (* for each neuron of the output layer *)
		let w = Array.make (network#layer_length (nbOfLayers-2)) 0. in
		for k = 0 to Array.length w -1 do
			w.(k) <- float_of_string (readLine file);
		done;
		let bias = float_of_string (readLine file) in
		network#s_neuron (nbOfLayers-1) i w bias;
	done;

	(*Printf.printf "%d\t%d\t%f\n" nbOfLayers nbOfInputs rate;*)

	close_in file;
	Printf.printf "Loaded from %s\n" filename;

	network;
end
