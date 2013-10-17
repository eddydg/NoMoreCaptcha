(* reseaux de neurones *)

(*let normalize_character_data src w h =
	let matrix = Array.make_matrix w h 0
	and srcX,srcY = Image_tools.get_dim src in
	for x = 0 to w-1 do
		for y = 0 to h-1 do
			Printf.printf "coucou"
		done;
	done;*)

type neuronState = INACTIVE | TRANSITION | ACTIVE


type information = {data : int; weight : float}

class neuron =
	object
		val mutable state = INACTIVE
		val mutable output = (0,0.)

		(** [effectue la sommation des produits des entrées par leur poids]  *)
		method sum () = 1+1;

		(** [fonction d'activation du neuronne]  *)
		method activation () = 1+1;
	end;;


class network =
	object
		val inLayer = []
		val layers = []
		val outLayer = []
		val inputs = []

		method train = 1

		method apply = 0;
	end;;