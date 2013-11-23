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




(* ----------- MAIN BOXES ---------- *)	

let toolbar = GButton.toolbar
	~orientation: `HORIZONTAL
	~style: `BOTH
	~packing: (box#pack ~expand: false) ()

let centerbox = GPack.hbox
	~packing: mainbox#add ()

let imagebox = GBin.scrolled_window
	~height: 300
	~width: 250
	~packing:centerbox#add ()

let textbox = GPack.vbox
	~packing:centerbox#add ()

let toolbox = GPack.hbox
	~spacing: 5
	~border_width: 10
	~packing:mainbox#add ()


let textarea =
  let scroll = GBin.scrolled_window
    ~hpolicy:`ALWAYS
    ~vpolicy:`ALWAYS
    ~shadow_type:`ETCHED_IN
    ~packing:textbox#add () in
  let txt = GText.view ~packing:scroll#add () in
  txt#misc#modify_font_by_name "Monospace 10";
  txt 




(* ----------- FILE OPERATIONS -------- *)

 let set_text s () =
  textarea#buffer#set_text s

let saveFile file = 
  let fileStream = open_out file in
  output_string fileStream (textarea#buffer#get_text ());
  close_out fileStream

let saveFileAs () =
  let title = "Save as" in
  let uri =
    GToolbox.input_string ~title ~text:"text_output.txt" ~ok:"Save" "" in
  match uri with
  | None -> ()
  | Some uri -> saveFile uri




(* ---------- IMAGE OPERATIONS --------- *)

let getImage () =
	if Array.length(Sys.argv) < 2 then
    	failwith "Image Missing!"
  	else
    	Sys.argv.(1)

let currentImg = ref (getImage ())


let bopenImage = GFile.chooser_button
	~action: `OPEN
	~packing: toolbar#add ()

let currentImage () = 
	let title = "Information" in
	match bopenImage#filename with 
		| Some s -> GToolbox.message_box ~title s
		| None -> GToolbox.message_box ~title "Pas d'image sélectionée."



let bselectedImage = 
	let button = GButton.button
		~label: "Image sélectionnée"
		~packing: toolbar#add () in
		button#connect#clicked ~callback:currentImage;
		button


let bSaveImage = 
	let button = GButton.button
		~stock: `SAVE 
		~packing: toolbar#add () in
		button#connect#clicked ~callback:saveFileAs;
		button

let showImage =
	let image = GMisc.image
		~file: !currentImg
		~packing: imagebox#add_with_viewport () in
		image




(* ----------- END TOOLBAR ----------- *)

let separator = GButton.separator_tool_item
	~packing: toolbar#insert ()


let bquit = 
	let button = GButton.button
		~label: "Quit"
		~packing: toolbar#add () in
		button#connect#clicked ~callback: GMain.quit;
		button




(* ---------- MAIN ------------ *)

let _ =
	window#connect#destroy
		~callback: GMain.quit;
	window#show ();
	GMain.main ()