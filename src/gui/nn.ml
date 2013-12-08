(* Init SDL *)
let sdl_init() = 
  begin
    Sdl.init[`EVERYTHING];
    Sdlevent.enable_events Sdlevent.all_events_mask;
  end

let rec wait_key () =
  let e = Sdlevent.wait_event () in
  match e with
  Sdlevent.KEYDOWN _ -> ()
  | _ -> wait_key ()

let image2liste src =
begin
    let w,h = Tools.get_dim src in
    let res = Array.make (w*h) 0. in
    for i = 0 to w -1 do
        for j = 0 to h -1 do
            res.(i*j) <- (let r,g,b = Sdlvideo.get_pixel_color src i j in
            if r > 127 && g > 127 && b > 127 then 1. else 0.);
        done;
    done;
    res;
end

let myPrint tab col =
begin
    let c = ref 0 in
    Printf.printf "[\n";
    for i = 0 to Array.length tab -1 do
        if tab.(i) = 0. then begin
            Printf.printf " ~ %!" end
        else (
            Printf.printf "%.0f~ %!" tab.(i));
        c := !c+1;
        if !c mod (col+1) = 0 then Printf.printf "\n%!";
    done;
    Printf.printf "\n]\n";
end

let auto_crop src =
begin
    let w,h = Tools.get_dim src in
    let x = ref 0 in let y = ref 0 in
    let posX = ref 0 in let posY = ref 0 in
    let posW = ref 0 in let posH = ref 0 in
    let isBlack = ref false in

    while !y <= h -1 && not !isBlack do (*from the top*)
        x := 0;
        while !x <= w -1 && not !isBlack do
            let r,g,b = Sdlvideo.get_pixel_color src !x !y in
            if r < 127 && g < 127 && b < 127 then
            begin
                isBlack := true;
                posY := !y;
            end else ();
            x := !x +1;
        done;
        y := !y +1;
    done;
    y := h -1;
    isBlack := false;
    while !y >= 0 && not !isBlack do (*from the bottom*)
        x := 0;
        while !x <= w -1 && not !isBlack do
            let r,g,b = Sdlvideo.get_pixel_color src !x !y in
            if r < 127 && g < 127 && b < 127 then
            begin
                isBlack := true;
                posH := (if !y <> h-1 then !y +1 else !y) - !posY;
            end else ();
            x := !x +1;
        done;
        y := !y -1;
    done;
    x := 0;
    isBlack := false;
    while !x <= w-1 && not !isBlack do (*from the left*)
        y := 0;
        while !y <= h-1 && not !isBlack do
            let r,g,b = Sdlvideo.get_pixel_color src !x !y in
            if r < 127 && g < 127 && b < 127 then
            begin
                isBlack := true;
                posX := !x;
            end else ();
            y := !y +1;
        done;
        x := !x +1;
    done;
    x := w-1;
    isBlack := false;
    while !x >= 0 && not !isBlack do (*from the right*)
        y := 0;
        while !y <= h-1 && not !isBlack do
            let r,g,b = Sdlvideo.get_pixel_color src !x !y in
            if r < 127 && g < 127 && b < 127 then
            begin
                isBlack := true;
                posW := (if !x <> w-1 then !x +1 else !x) - !posX;
            end else ();
            y := !y +1;
        done;
        x := !x -1;
    done;
    let dst = Sdlvideo.create_RGB_surface_format src [] !posW !posH in
    Sdlvideo.blit_surface ~src:src ~src_rect:(Sdlvideo.rect !posX !posY !posW !posH) ~dst:dst ();
    dst;
end

let gen_examples charset fonts size col row =
begin
    let res = Array.init (Array.length charset * Array.length fonts) (fun _ -> (Array.make  0 0., -1)) in
    let count = ref 0 in
    for i = 0 to Array.length fonts -1 do (* for each font *)
        Printf.printf "Loading %s...\n" fonts.(i);
        let myfont = Sdlttf.open_font fonts.(i) size in
        for j = 0 to Array.length charset -1 do (* for each letter *)
            let tmp = Sdlttf.render_utf8_blended myfont charset.(j) ~fg:Sdlvideo.black in (*texte*)
            let tw,th = Sdlttf.size_text myfont charset.(j) in
            let pic = Sdlvideo.create_RGB_surface_format tmp [] (tw+1) (th+1) in
            Sdlvideo.fill_rect pic (Int32.of_int 0xFFFFFF);
            Sdlvideo.blit_surface ~src:tmp ~dst:pic ~dst_rect:(Sdlvideo.rect 0 0 0 0) ();

(*
            let w,h = Tools.get_dim (pic) in
            let surface = Sdlvideo.set_video_mode w h [`DOUBLEBUF] in
            Tools.print_image (pic) surface;
            (*wait_key ();*)
*)
            res.(!count) <- (Network.normalize (pic) col row, j);
            count := !count +1;
        done;
    done;
    res;
end

let gen_texte text font_name size=
begin
    let myfont = Sdlttf.open_font font_name size in
    let tmp = Sdlttf.render_utf8_blended myfont text ~fg:Sdlvideo.black in (*texte*)
    let tw,th = Sdlttf.size_text myfont text in
    let pic = Sdlvideo.create_RGB_surface_format tmp [] (tw+1) (th+1) in
    Sdlvideo.fill_rect pic (Int32.of_int 0xFFFFFF);
    Sdlvideo.blit_surface ~src:tmp ~dst:pic ~dst_rect:(Sdlvideo.rect 0 0 0 0) ();

    let w,h = Tools.get_dim (pic) in
    let surface = Sdlvideo.set_video_mode w h [`DOUBLEBUF] in
    Tools.print_image (pic) surface;
    wait_key ();
end

let is_white img =
begin
    let w,h = Tools.get_dim img in
    let isWhite = ref true in
    for x = 0 to w -1 do
        for y = 0 to h-1 do
            isWhite := !isWhite && (Tools.level (Sdlvideo.get_pixel_color img x y) > 200.);
        done;
    done;
    !isWhite;
end

let read_img img charset=
begin
    (*let (w2,h2) = Tools.get_dim img in
    let surface2 = Sdlvideo.set_video_mode w2 h2 [`DOUBLEBUF] in
    Tools.print_image img surface2;
    wait_key();*)
    let result = ref "" in
    let net = Network.load "savedNN/arial.txt" in
    let list1 = Characters.cross_img img in
    for i = 0 to List.length list1 -1 do
        for j = 0 to List.length (List.nth list1 i) -1 do
            for k = 0 to List.length (List.nth (List.nth list1 i) j) -1 do
                let carac = (List.nth (List.nth (List.nth list1 i) j) k) in
                (*let (w2,h2) = Tools.get_dim carac in
                let surface2 = Sdlvideo.set_video_mode w2 h2 [`DOUBLEBUF] in
                Tools.print_image carac surface2;
                wait_key();*)
                if not (is_white carac) then begin
                    let tab = Network.normalize carac 12 12 in
                let p,out = net#run (tab) in
                myPrint tab 12; (*sert a rien *)
                Printf.printf "letter: %s\n%!" charset.(p) ;
                result := charset.(p)^(!result);
                end;
            done;
            result := " "^(!result);
        done;
        result := "\n"^(!result);
    done;
    (!result);
end

let main () =
begin
    if Array.length (Sys.argv) < 2 then
      failwith "Need a picture";
    sdl_init ();
    Sdlttf.init();
    (*Basic instructions that show the picture*)

    let img = Sdlloader.load_image Sys.argv.(1) in

    let col = 12 in let row = col in
    let charset = [|"0";"1";"2";"3";"4";"5";"6";"7";"8";"9";(*"<";">";",";";";":";"!";"?";".";"/";"§";"%";"£";"€";"&";"ç";"(";")";"[";"]";"{";"}";"=";"+";"-";"*";"/";*)
    "B";"A";"C";"D";"E";"F";"G";"H";"I";"J";"K";"L";"M";"N";"O";"P";"Q";"R";"S";"T";"U";"V";"W";"X";"Y";"Z"
    ;"a";"b";"c";"d";"e";"f";"g";"h";"i";"j";"k";"l";"m";"n";"o";"p";"q";"r";"s";"t";"u";"v";"w";"x";"y";"z"(**)|] in
    let fontList = [|"fonts/arial.ttf"(*; "fonts/times.ttf"; "fonts/bell.ttf";"fonts/calibrii.ttf";
                        "fonts/cour.ttf";"fonts/gara.ttf";"fonts/kokila.ttf";"fonts/pala.ttf";"fonts/rod.ttf";"fonts/vani.ttf"*)|] in
    
    (*gen_texte "Bonjour\ndoudou" "fonts/arial.ttf" 100;*)
    let texteFinal = read_img img charset in
    Printf.printf "%s\n" texteFinal;

    let gridOfImg = Network.normalize img col row in
    myPrint gridOfImg col;
    let myEx,_ = (gen_examples [|Sys.argv.(2)|] [|"fonts/arial.ttf"|] 25 col row ).(0) in
    myPrint myEx col;

    let (w,h) = Tools.get_dim img in
    let surface = Sdlvideo.set_video_mode w h [`DOUBLEBUF] in
    Tools.print_image img surface;
    (*wait_key();*)

    (*let examples = gen_examples charset [|"fonts/arial.ttf"|] 25 col row in
    let reseau = new Network.network in
    reseau#make ((col+1)*(row+1))   [|(col+1)*(row+1); Array.length charset|]  0.5;
	(*Train*)
    Printf.printf "Learning...\n";
    for i = 0 to 2500 do
        for j = 0 to Array.length examples -1 do
            let ex,pos = examples.(j) in
            let wantedResult = Array.make (Array.length charset) 0. in
            wantedResult.(pos) <- 1.;
            reseau#train ex wantedResult;
        done;
        if i mod 50 = 0 then
            Printf.printf "i : %d\n%!" i;
    done;
    reseau#save "savedNN/reseau.txt";*)
    (*Show results*)
    let reseau2 = Network.load "savedNN/reseau.txt" in
    let p,out = reseau2#run gridOfImg in
    Printf.printf("result: %s = %.2f%%\n") charset.(p) (out.(p)*.100.);
    Array.iter ( fun e -> Printf.printf "%.2f\t" (e*.100.)) out;
    Printf.printf "\n";
    let p,out = reseau2#run myEx in
    Printf.printf("perfect result: %s = %.2f%%\n") charset.(p) (out.(p)*.100.);
    (*Array.iter ( fun e -> Printf.printf "%.2f\t" (e*.100.)) out;*)
    Printf.printf "\n";
    
    Sdlttf.quit ();
    texteFinal;
end
