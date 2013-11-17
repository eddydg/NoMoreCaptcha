let _ = GMain.init ()

let defaultPic = "text.bmp"

let help_message () = print_endline "Coucou"

let window = GWindow.window
	~height: 500
	~width: 700
	~position: `CENTER
	~title: "NoMoreCaptcha" ()

let mainbox = GPack.vbox
	~packing: window#add ()

let box = GPack.vbox
	~packing:mainbox#add ()

(*
let everythingAfter delim_char str = 
	let i = ref 0 in
	while !i < String.length str && str.[!i] != delim_char do
		incr i;
	done;
	if !i < String.length str then
		String.sub str (!i+1) ((String.length str) - (!i+1))
	else
		""
*)


(* ----------- MAIN BOXES ---------- *)	

let toolbar = GButton.toolbar
	~orientation: `HORIZONTAL
	~style: `BOTH
	~packing: (box#pack ~expand: false) ()

let centerbox = GPack.hbox
	~packing: mainbox#add ()


let imagebox = GPack.vbox
	~height: 300
	~width: 250
	~packing:centerbox#add ()

let textbox = GPack.vbox
	~packing:centerbox#add ()

let toolbox = GPack.hbox
	~spacing: 5
	~border_width: 10
	~packing:mainbox#add ()




(* ----------- TOOLBAR ----------- *)


let openImage = GFile.chooser_button
	~action: `OPEN
	~packing: toolbar#add ()

let saveImage = GButton.tool_item
	~packing: toolbar#insert ()


(*
let itemsOrder = [`B `NEW ]

let _ =
	let packing = toolbar#insert in
	List.iter (function
		| `S -> ignore (GButton.separator_tool_item ~packing ())
		| `B stock ->
			let id = GtkStock.convert_id stock in
			let id = everythingAfter '-' id in
			let id = String.capitalize id in
			ignore (GButton.tool_button ~label:id ~stock ~packing ())
		| `T label ->  ignore (GButton.toggle_tool_button ~label ~packing ())
		| `M menu ->  ignore (GButton.menu_tool_button ~label:"Menu" ~menu ~packing ())
		| _ -> ()
	) itemsOrder
*)

(* ------------ ADDING ------------- *)

let image = GMisc.image
	~file: defaultPic
	~packing: (imagebox#pack ~expand: false) ()


let entry = GText.view
	~width: 100
	~height: 300
	~border_width: 10
	~packing: (textbox#pack ~expand: false) ()	


(*
let bRotate = GButton.button
	~label: "Rotate"
	~packing: toolbox#pack ()

let bToGrey = GButton.button
	~label: "Grey"
	~packing: toolbox#pack ()
*)

let currentImage () = match openImage#filename with 
	| Some s -> print_endline s
	| None -> print_endline "Pas d'image sélectionée."

let bOpenImage = 
	let button = GButton.button
		~label: "Image sélectionnée"
		~packing: openImage#add () in
		button#connect#clicked ~callback:currentImage;
		button


let bSaveImage = GButton.button
		~stock: `SAVE
		~packing: saveImage#add ()



let separator = GButton.separator_tool_item
	~packing: toolbar#insert ()


let bquit = 
	let button = GButton.button
		~label: "Quit"
		~packing: toolbar#add () in
		button#connect#clicked ~callback: GMain.quit;
		button



(* --------------------------------- *)


let _ =
	window#connect#destroy
		~callback: GMain.quit;
	window#show ();
	GMain.main ()