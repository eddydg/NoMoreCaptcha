(*1) initialize all weights and biases with random values between 0 and 1
2) calculate the output of the network
3) calculate the global error
4) adjust the weights of the output neuron using the global error
5) calculate the hidden neurons' errors (split the global error)
6) adjust the hidden neurons' weights using their errors
7) go to step 2) and repeat this until the error gets minimal (about 2000 loop for xor)*)

let sigmoidOutput x = 1. /. (1. +. exp (-1.*.x))

let sigmoidDeriavtive x = x *. (1. -. x)

let toPositive x =
	match x with
	| x when x < 0. -> -1. *. x
	| _ -> x

class neuron =
object
	val mutable inputs = Array.make 2 0.;
	val mutable weights = Array.make	2 0.;
	val mutable error = 1.;
	val mutable biasWeight = 0.;

	method get_Output () =
	sigmoidOutput (weights.(0) *. inputs.(0) +. weights.(1) *. inputs.(1) +. biasWeight)

	method get_weight i = weights.(i)	

	method randomize_Weight () =
	begin
		weights.(0) <- Random.float 1.;
		weights.(1) <- Random.float 1.;
		biasWeight <- Random.float 1.;
	end

	method adjust_Weight () = 
	begin
		weights.(0) <- weights.(0) +. error *. inputs.(0);
		weights.(1) <- weights.(1) +. error *. inputs.(1);
		biasWeight <- biasWeight +. error;
	end

	method set_inputs a b=
	begin
		inputs.(0) <- a;
		inputs.(1) <- b;
	end

	method set_error e = error <- e
	method get_error () = error
end

let train () =
begin
	let inputs = Array.make_matrix	4 2 0. in
	inputs.(0).(0) <- 0.; inputs.(0).(1) <- 0.;
	inputs.(1).(0) <- 0.; inputs.(1).(1) <- 1.;
	inputs.(2).(0) <- 1.; inputs.(2).(1) <- 0.;
	inputs.(3).(0) <- 1.; inputs.(3).(1) <- 1.;

	let results = Array.make 4 0. in
	results.(0) <- 0.; results.(1) <- 1.; results.(2) <- 1.; results.(3) <- 0.;
	
	(*creating new neurons*)
	let hiddenNeuron1 = new neuron in
	let hiddenNeuron2 = new neuron in
	let outputNeuron = new neuron in	

	(*initialize neurons' weight*)
	hiddenNeuron1#randomize_Weight();
	hiddenNeuron2#randomize_Weight();
	outputNeuron#randomize_Weight();

	let count = ref 0 in
	Printf.printf "%f\n" (outputNeuron#get_error());
	while !count < 5000 do
		for i = 0 to Array.length inputs -1 do
			hiddenNeuron1#set_inputs inputs.(i).(0) inputs.(i).(1);
			hiddenNeuron2#set_inputs inputs.(i).(0) inputs.(i).(1);

			outputNeuron#set_inputs (hiddenNeuron1#get_Output()) (hiddenNeuron2#get_Output());

			let out = outputNeuron#get_Output() in
			(*Printf.printf "%f xor %f = %f\t\terr: %f\n" inputs.(i).(0) inputs.(i).(1) out (outputNeuron#get_error());*)
			(*Printf.printf "out error : %f\n" (outputNeuron#get_error());*)
			
			outputNeuron#set_error ((sigmoidDeriavtive out) *. (results.(i) -. out));
			outputNeuron#adjust_Weight();

			hiddenNeuron1#set_error ((sigmoidDeriavtive (hiddenNeuron1#get_Output())) *. (outputNeuron#get_error()) *. (outputNeuron#get_weight 0));
			hiddenNeuron2#set_error ((sigmoidDeriavtive (hiddenNeuron2#get_Output())) *. (outputNeuron#get_error()) *. (outputNeuron#get_weight 1));

			hiddenNeuron1#adjust_Weight ();
			hiddenNeuron2#adjust_Weight ();			
		done;		
		count := !count+1;
	done;
	Printf.printf "\ncount : %d\n" !count;
	for i = 0 to Array.length inputs -1 do
			hiddenNeuron1#set_inputs inputs.(i).(0) inputs.(i).(1);
			hiddenNeuron2#set_inputs inputs.(i).(0) inputs.(i).(1);

			outputNeuron#set_inputs (hiddenNeuron1#get_Output()) (hiddenNeuron2#get_Output());

			let out = outputNeuron#get_Output() in
			Printf.printf "%f xor %f = %f\t\terr: %f\n" inputs.(i).(0) inputs.(i).(1) out (outputNeuron#get_error());
			(*Printf.printf "out error : %f\n" (outputNeuron#get_error());*)
			
			outputNeuron#set_error (sigmoidDeriavtive (out *. (results.(i) -. out)));
			outputNeuron#adjust_Weight();

			hiddenNeuron1#set_error ((sigmoidDeriavtive (hiddenNeuron1#get_Output())) *. (outputNeuron#get_error()) *. (outputNeuron#get_weight 0));
			hiddenNeuron2#set_error ((sigmoidDeriavtive (hiddenNeuron2#get_Output())) *. (outputNeuron#get_error()) *. (outputNeuron#get_weight 1));

			hiddenNeuron1#adjust_Weight ();
			hiddenNeuron2#adjust_Weight ();			
		done;

end


(*
let main () =
begin
	Printf.printf "Start\n";
	Random.self_init();
	train();
	Printf.printf "End\n";
end

let _ = main ()*)