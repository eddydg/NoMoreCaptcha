let sigmoid x = 1. /. (1. +. exp (-1. *. x))

type initial = {
iter : int;
learnRate : float;

mutable input1 : float;
mutable input2 : float;
mutable target : float;

(*The neural network variables (the neurons and the weights "biasweight"*)
mutable hidden1 : float;
mutable hidden2 : float;
mutable output1 : float;

mutable weight_i1_h1 : float;
mutable weight_i1_h2 : float;
mutable weight_i2_h1 : float;
mutable weight_i2_h2 : float;
mutable weight_h1_o1 : float;
mutable weight_h2_o1 : float}

let create_init () =begin {
	iter = 10000;
	learnRate = 0.5;

	input1 = 0.;
	input2 = 0.;
	target = 0.;

	(*The neural network variables (the neurons and the weights "biasweight"*)
	hidden1 = Random.float 1.;
	hidden2 = Random.float 1.;
	output1 = Random.float 1.;

	weight_i1_h1 = Random.float 1.;
	weight_i1_h2 = Random.float 1.;
	weight_i2_h1 = Random.float 1.;
	weight_i2_h2 = Random.float 1.;
	weight_h1_o1 = Random.float 1.;
	weight_h2_o1 = Random.float 1.} end

let train (a:float) (b:float) (result:float) (w:initial)=
begin
	w.input1 <- a;
	w.input2 <- b;
	w.target <- result;

	let output_hidden1 = ref 0. in
    let output_hidden2 = ref 0. in
    let output_output1 = ref 0. in

	(*Calculate the outputs*)
    output_hidden1:= w.input1 *. w.weight_i1_h1 +. w.input2 *. w.weight_i2_h1 +. w.hidden1;
    output_hidden1:=  sigmoid(!output_hidden1);

    output_hidden2 := w.input1 *. w.weight_i1_h2 +. w.input2 *. w.weight_i2_h2 +. w.hidden2;
    output_hidden2 := sigmoid(!output_hidden2);

    output_output1:= !output_hidden1 *. w.weight_h1_o1 +. !output_hidden2 *. w.weight_h2_o1 +. w.output1;
    output_output1 := sigmoid(!output_output1);

    (*Calculate the error*)
    let output1_adjustment = !output_output1 *. (1. -. !output_output1) *. (w.target -. !output_output1) in
    let hidden2_adjustment = !output_hidden2 *. (1. -. !output_hidden2) *. output1_adjustment *. w.weight_h2_o1 in
    let hidden1_adjustment = !output_hidden1 *. (1. -. !output_hidden1) *. output1_adjustment *. w.weight_h1_o1 in

	(*Adjust the weights*)
    w.weight_i1_h1 <- w.weight_i1_h1 +. w.learnRate *. hidden1_adjustment *. w.input1;
    w.weight_i1_h2 <- w.weight_i1_h2 +. w.learnRate *. hidden2_adjustment *. w.input1;
    w.weight_i2_h1 <- w.weight_i2_h1 +. w.learnRate *. hidden1_adjustment *. w.input2;
    w.weight_i2_h2 <- w.weight_i2_h2 +. w.learnRate *. hidden2_adjustment *. w.input2;
    w.weight_h1_o1 <- w.weight_h1_o1 +. w.learnRate *. output1_adjustment *. !output_hidden1; 
    w.weight_h2_o1 <- w.weight_h2_o1 +. w.learnRate *. output1_adjustment *. !output_hidden2;     
    w.hidden1 <- w.hidden1 +. w.learnRate *. hidden1_adjustment; 
    w.hidden2 <- w.hidden2 +. w.learnRate *. hidden2_adjustment; 
    w.output1 <- w.output1 +. w.learnRate *. output1_adjustment
end


let run (a:float) (b:float) (w:initial)=
begin
	w.input1 <- a;	
    w.input2<-  b;	

    let output_hidden1 = ref 0. in
    let output_hidden2 = ref 0. in
    let output_output1 = ref 0. in
	
    (*Calculate the outputs, same code as the train function*)
    output_hidden1:= w.input1 *. w.weight_i1_h1 +. w.input2 *. w.weight_i2_h1 +. w.hidden1;
    output_hidden1:=  sigmoid(!output_hidden1);

    output_hidden2 := w.input1 *. w.weight_i1_h2 +. w.input2 *. w.weight_i2_h2 +. w.hidden2;
    output_hidden2 := sigmoid(!output_hidden2);

    output_output1:= !output_hidden1 *. w.weight_h1_o1 +. !output_hidden2 *. w.weight_h2_o1 +. w.output1;
    output_output1 := sigmoid(!output_output1);
    
    (*Show the result*)
    Printf.printf "%d XOR %d = %f  =  " (int_of_float a) (int_of_float b) !output_output1;
    Printf.printf "%d\n" (if !output_output1 >= 0.5 then 1 else 0);
end

let trainAndRun () =
begin
	Random.self_init ();
	(*Train*)
	let w = create_init() in
    for i = 0 to w.iter do
    	train 0. 0. 0. w;
    	train 0. 1. 1. w ;
    	train 1. 0. 1. w ;
    	train 1. 1. 0. w ;
    done;
    (*Show results*)
    run 0. 0. w ;
	run 0. 1. w ;
	run 1. 0. w ;
	run 1. 1. w ;
end
