(* Sigmoid:  Activation fonction of the neural network *)
let sigmoid x = 1. /. (1. +. exp (-1. *. x))

(* create a random value between -1 and 1. usefull to initialize weights *)
let myRandom () = (if Random.bool () then 1. else -1.) *. Random.float 0.5

class neuron =
object
	val mutable inputs = Array.make 0 0.;
	val mutable weights = Array.make 0 (myRandom());
	val mutable adjustement = 0.;
	val mutable biasweight = (myRandom());

	method g_bias () = biasweight
	method s_bias x = biasweight <- x

	method s_inputs tab =
	begin
		if Array.length tab <> Array.length inputs then
			begin
				failwith "network.ml : (s_inputs) invalid inputs";
			end
		else
			begin
				for i = 0 to Array.length inputs -1 do
					inputs.(i) <- tab.(i);
				done;
			end
	end

	method g_input n =
	begin
		if n < Array.length inputs then begin
			inputs.(n)	end
		else begin
			failwith "network.ml : (g_inputs) index out of bound"; end
	end

	method s_adjustment x = adjustement <- x

	method g_adjustment () = adjustement

	method g_weight n =
	begin
		if n < Array.length weights then begin
			weights.(n)	end
		else begin
			failwith "network.ml : (g_weight) index out of bound"; end
	end

	method g_all_weights () = weights

	method g_output () =
	begin
		let tmp = ref 0. in
		for i = 0 to Array.length inputs -1 do
			tmp := !tmp +. weights.(i) *. inputs.(i);
		done;
		sigmoid (!tmp +. biasweight);
	end

	method init n = (*n is the number of inputs and it randomize all wait and bias between -1 and 1*)
	begin
		inputs <- Array.make n 0.;
		weights <- Array.make n 0.;
		for i = 0 to n-1 do
			weights.(i) <- myRandom();
		done;
		biasweight <- myRandom();
	end

	method adjust_weight rate =
	begin
		for i = 0 to Array.length weights -1 do
			weights.(i) <- weights.(i) +. rate *. adjustement *. inputs.(i);
		done;
		biasweight <- biasweight +. rate *. adjustement;
	end

	method s_weights tab =
	begin
		if Array.length tab <> Array.length weights then
			begin
				failwith "network.ml : (s_weights) invalid weights";
			end
		else
			begin
				for i = 0 to Array.length inputs -1 do
					weights.(i) <- tab.(i);
				done;
			end
	end
end

(*return an array contening output of each neuron of the layer 'layer' *)
let out_of_layer (layer: neuron array) =
begin
	let result = Array.make (Array.length layer) 0. in
	for i = 0 to Array.length layer -1 do
		result.(i) <- layer.(i)#g_output ();
	done;
	result;
end
