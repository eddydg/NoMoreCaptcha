(*fichier servant à definir les differentes matrices de convolutions*)

let repoussage () =
let m = Array.make_matrix 3 3 0 in
	m.(0).(0) <- -2;
	m.(0).(1) <- -1;
	m.(0).(2) <- 0;
	m.(1).(0) <- -1;
	m.(1).(1) <- 1;
	m.(1).(2) <- 1;
	m.(2).(0) <- 0;
	m.(2).(1) <- 1;
	m.(2).(2) <- 2;
	m;