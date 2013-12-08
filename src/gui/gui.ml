let _ = GMain.init ()

let defaultPic = ref "text.bmp"


let help_message () = print_endline "Coucou"

let window = GWindow.window
	~height: 600
	~width: 750
	~position: `CENTER
	~title: "NoMoreCaptcha " ()

let mainbox = GPack.vbox
	~packing: window#add ()


(* ----------- MAIN BOXES ---------- *)	

let toolbar = GButton.toolbar
	~orientation: `HORIZONTAL
	~style: `BOTH
	~packing: (mainbox#pack ~expand: false) ()

let centerbox = GPack.hbox
	~packing: mainbox#add ()

let imagebox = GBin.scrolled_window
	~height: 500
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
	GtkSpell.attach ~lang:"fr_FR" txt;
	txt 


(* ----------- FILE OPERATIONS -------- *)
let getImage () =
	if Array.length(Sys.argv) < 2 then
    	failwith "Image Missing!"
  	else
    	Sys.argv.(1)

let currentImg = ref (getImage ())
let currentAngle = ref 0


 let set_text s () =
  textarea#buffer#set_text s

let saveFile file = 
  let fileStream = open_out file in
  output_string fileStream (textarea#buffer#get_text ());
  close_out fileStream

let saveFileAs () =
  let title = "Save as" in
  let uri =
    GToolbox.input_string ~title ~text:"saves/text_output.txt" ~ok:"Save" "" in
  match uri with
  | None -> ()
  | Some uri -> saveFile uri

 let saveImageAs () =
  let title = "Save as" in
  let uri =
    GToolbox.input_string ~title ~text:"saves/new_image.bmp" ~ok:"Save" "" in
  match uri with
  | None -> ()
  | Some uri -> let image = Sdlloader.load_image (!currentImg) in
  				Sdlvideo.save_BMP image uri



(* ---------- IMAGE OPERATIONS --------- *)

let bSaveText = 
	let button = GButton.button
		~label:"Save Text"
		~packing: toolbar#add () in
		GMisc.image ~stock:`SAVE ~packing:button#set_image ();
		button#connect#clicked ~callback:saveFileAs;
		button

let bSaveImage = 
	let button = GButton.button
		~label:"Save Image"
		~packing: toolbar#add () in
		GMisc.image ~stock:`SAVE ~packing:button#set_image ();
		button#connect#clicked ~callback:saveImageAs;
		button

let showImage =
	let image = GMisc.image
		~file: !currentImg
		~packing: imagebox#add_with_viewport () in
		image

let updateImage img =
	showImage#set_file img;
	currentImg := img

let setImage btn () =
	Gaux.may showImage#set_file btn#filename;
	match btn#filename with
	| None -> ()
	| Some pic -> currentImg := pic; defaultPic := pic

let bopenImage = 
	let button = GFile.chooser_button
		~title:"Select Image"
		~width:120
		~action: `OPEN
		~packing: toolbar#add () in
		button#connect#selection_changed ~callback:(setImage button);
		button

let currentImage () = 
	let title = "Information" in
	match bopenImage#filename with 
		| Some s -> GToolbox.message_box ~title s
		| None -> GToolbox.message_box ~title "No new image."

let bselectedImage = 
	let button = GButton.button
		~label: "Current Image"
		~packing: toolbar#add () in
		button#connect#clicked ~callback:currentImage;
		button

(* let bselectColor =
	let dialog = GWindow.color_selection_dialog
		~parent:window
		~destroy_with_parent:true
		~position:`CENTER_ON_PARENT () in

		dialog#ok_button#connect#clicked (fun () ->
			textarea#misc#modify_base [`NORMAL, `COLOR dialog#colorsel#color]
		);
		let button = GButton.button
			~label:"Background color"
			~packing:toolbox#add () in
		GMisc.image ~stock:`COLOR_PICKER ~packing:button#set_image ();
		button#connect#clicked (fun () ->
			ignore (dialog#run ());
			dialog#misc#hide ()
		);
		button *)


(* ---------- PROCESSING --------- *)

let detectedAngle = ref 0.

let detectAngle () =
	let pic = Sdlloader.load_image(!currentImg) in
	let angle = Rotation.get_angle pic in
	let title = "Angle Detection" in
	GToolbox.message_box
		~title ("The picture is at "^(string_of_float angle)^" degrees.");
	detectedAngle := angle

let bdetectAngle =
	let button = GButton.button
		~label: "Detect Angle"
		~packing: toolbox#add () in
		button#connect#clicked ~callback:detectAngle;
		button


(* ---------- ROTATION ------------*)

let setRotate spinner () = 
	detectedAngle := (float_of_int spinner#value_as_int);
	let pic = Sdlloader.load_image (!currentImg) in
	let pic = Rotation.rotation pic (-1.*.(!detectedAngle)) in
	Sdlvideo.save_BMP pic "output.bmp";
	Sdl.quit ();
	updateImage "output.bmp"

let wimageRotate () =
	let dialog = GWindow.dialog
		~parent:window
		~title:"Rotation"
		~height:140
		~width:200 () in
	let label = GMisc.label ~text:"Rotating at: " ~packing:dialog#vbox#add () in
	let adj = GData.adjustment
		~value:(!detectedAngle)
		~lower:0.0
		~upper:90.0
		~step_incr:1.0
		~page_incr:1.0
		~page_size:0.0 () in
	let spinner = GEdit.spin_button
		~adjustment:adj
		~rate:1.0
		~digits:2
		~width:100
		~packing:dialog#vbox#add () in
	let ok = GButton.button
		~label:"Rotate"
		~packing:dialog#vbox#add () in
	let cancel = GButton.button
		~label:"Quit"
		~packing:dialog#vbox#add () in
	cancel#connect#clicked ~callback:(dialog#misc#hide);
	ok#connect#clicked ~callback:(setRotate spinner);
	ignore (dialog#run ());
	dialog#misc#hide ()

let bimageRotate =
	let button = GButton.button
		~label: "Rotate"
		~packing: toolbox#add () in
		button#connect#clicked ~callback:wimageRotate;
		button


(* ----------- FILTERS -----------*)

let noNoiseAverage () =
	let pic = Sdlloader.load_image(!currentImg) in
	Fonctions.noNoise_average pic;
	Sdlvideo.save_BMP pic "output.bmp";
	Sdl.quit ();
	updateImage "output.bmp"

(* let noNoiseMedian threshold () =
	let pic = Sdlloader.load_image(!currentImg) in
	Fonctions.noNoise_median pic;
	Sdlvideo.save_BMP pic "output.bmp";
	Sdl.quit ();
	updateImage "output.bmp"

let noNoise spinner isAverage =
	let step = spinner#value_as_int in
	if isAverage then
		noNoiseAverage step
	else
		noNoiseMedian step


let wnoNoise () =
	let dialog = GWindow.dialog
		~parent:window
		~title:"Go away noise!"
		~height:160
		~width:200 () in
	let label = GMisc.label ~text:"Noise removing threshold: " ~packing:dialog#vbox#add () in
	let adj = GData.adjustment
		~value:0.0
		~lower:0.0
		~upper:10.0
		~step_incr:1.0
		~page_incr:1.0
		~page_size:0.0 () in
	let spinner = GEdit.spin_button
		~adjustment:adj
		~rate:1.0
		~digits:2
		~width:100
		~packing:dialog#vbox#add () in
	let box = GPack.hbox
		~spacing:10
		~border_width:10
		~packing:dialog#vbox#add () in
	let bAverage = GButton.button
		~label:"Average"
		~packing:dialog#vbox#add () in
 	let bMedian = GButton.button
		~label:"Median"
		~packing:dialog#vbox#add () in
	let cancel = GButton.button
		~label:"Quit"
		~packing:dialog#vbox#add () in
	cancel#connect#clicked ~callback:(dialog#misc#hide);
	bAverage#connect#clicked ~callback:(noNoise spinner true);
	bMedian#connect#clicked ~callback:(noNoise spinner false);
	ignore (dialog#run ());
	dialog#misc#hide ()
*)

let bnoNoise =
	let button = GButton.button
	~label: "Noise Remove"
	~packing: toolbox#add () in
	button#connect#clicked ~callback:noNoiseAverage;
	button


let seuilDetection adj () =
	let pic = Sdlloader.load_image (!currentImg) in
	let seuil = Fonctions.seuil pic in
	adj#set_value (float_of_int seuil)


let blackAndWhite spinner () = 
	let threshold = (float_of_int spinner#value_as_int) in
	let pic = Sdlloader.load_image(!currentImg) in
 	Fonctions.blackAndWhite2 pic ?vraiseuil:(Some threshold);
	Sdlvideo.save_BMP pic "output.bmp";
	Sdl.quit ();
	updateImage "output.bmp"

let wblackAndWhite () =
	let dialog = GWindow.dialog
		~parent:window
		~title:"BlackAndWhite"
		~height:160
		~width:200 () in
	let label = GMisc.label ~text:"Black and White threshold: " ~packing:dialog#vbox#add () in
	let adj = GData.adjustment
		~value:0.0
		~lower:0.0
		~upper:255.0
		~step_incr:1.0
		~page_incr:1.0
		~page_size:0.0 () in
	let spinner = GEdit.spin_button
		~adjustment:adj
		~rate:1.0
		~digits:3
		~width:100
		~packing:dialog#vbox#add () in
	let ok = GButton.button
		~label:"Apply"
		~packing:dialog#vbox#add () in
	let auto = GButton.button
		~label:"Auto"
		~packing:dialog#vbox#add () in
	let cancel = GButton.button
		~label:"Quit"
		~packing:dialog#vbox#add () in
	cancel#connect#clicked ~callback:(dialog#misc#hide);
	ok#connect#clicked ~callback:(blackAndWhite spinner);
	auto#connect#clicked ~callback:(seuilDetection adj);
	ignore (dialog#run ());
	dialog#misc#hide ()

let bblackAndWhite =
	let button = GButton.button
	~label: "Black And White"
	~packing: toolbox#add () in
	button#connect#clicked ~callback:(wblackAndWhite)


(* ---------- DETECTION ----------- *)

let carDetection () = 
	let pic = Sdlloader.load_image(!currentImg) in
	Detect.circle_this pic;
	Sdlvideo.save_BMP pic "detect_output.bmp";
	Sdl.quit ();
	showImage#set_file "detect_output.bmp"

let bcarDetection =
	let button = GButton.button
		~label: "Detect"
		~packing: toolbox#add () in
		button#connect#clicked ~callback:carDetection;
		button


let carRecognition () =
	let pic = Sdlloader.load_image(!currentImg) in
	let charset = [|"0";"1";"2";"3";"4";"5";"6";"7";"8";"9";(*"<";">";",";";";":";"!";"?";".";"/";"§";"%";"£";"€";"&";"ç";"(";")";"[";"]";"{";"}";"=";"+";"-";"*";"/";*)
    "B";"A";"C";"D";"E";"F";"G";"H";"I";"J";"K";"L";"M";"N";"O";"P";"Q";"R";"S";"T";"U";"V";"W";"X";"Y";"Z"
    ;"a";"b";"c";"d";"e";"f";"g";"h";"i";"j";"k";"l";"m";"n";"o";"p";"q";"r";"s";"t";"u";"v";"w";"x";"y";"z"|] in
	let text = Nn.read_img pic charset in
	set_text text ()

let bcarRecognition =
	let button = GButton.button
		~label:"Convert to text"
		~packing: toolbox#add () in
		button#connect#clicked ~callback:carRecognition;
		button


(* ----------- MISC ----------- *)

let undo () = updateImage (!defaultPic)

let bundo =
	let button = GButton.button
		~label: "Undo"
		~packing: toolbar#add () in
		button#connect#clicked ~callback:undo;
		button

let separator = GButton.separator_tool_item
	~packing: toolbar#insert ()

let bAboutUs =
	let dialog = GWindow.about_dialog
		~authors:["MeltedPenguin\nNicompleX\nJiiu\nGuerano"]
		~copyright:"copyright @ 2013-2014"
		~version:"v1."
		~website:"http://nomorecaptcha.free.fr/"
		~website_label:"NoMoreCaptcha"
		~parent:window
		~position:`CENTER_ON_PARENT
		~destroy_with_parent:true () in
	let button = GButton.button
		~label:"About Us"
		~packing:toolbar#add () in
	GMisc.image ~stock:`ABOUT ~packing:button#set_image ();
	button#connect#clicked (fun () ->
		ignore (dialog#run ());
		dialog#misc#hide ()
	);
	button

let bquit = 
	let button = GButton.button
		~label: "Quit"
		~packing: toolbar#add () in
		GMisc.image ~stock:`QUIT ~packing:button#set_image ();
		button#connect#clicked ~callback: GMain.quit;
		button


(* ---------- MAIN ------------ *)

let _ =
	window#connect#destroy
		~callback: (GMain.quit);
	window#show ();
	GMain.main ()
