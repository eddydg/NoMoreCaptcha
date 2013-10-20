(***Initialisation des fonctions SDL***)

(*mensions d'une image *)
let get_dims img =
  ((Sdlvideo.surface_info img).Sdlvideo.w, (Sdlvideo.surface_info img).Sdlvideo.h)
 
(* init de SDL *)
let sdl_init () =
  begin
    Sdl.init [`EVERYTHING];
    Sdlevent.enable_events Sdlevent.all_events_mask;
  end
 
(* attendre une touche ... *)
let rec wait_key () =
  let e = Sdlevent.wait_event () in
    match e with
    Sdlevent.KEYDOWN _ -> ()
      | _ -> wait_key ()
 
(*
  show img dst
  affiche la surface img sur la surface de destination dst (normalement l'écran)
*)
let show img dst =
  let d = Sdlvideo.display_format img in
    Sdlvideo.blit_surface d dst ();
    Sdlvideo.flip dst

(***Initialisation des fonctions de detections***)

(*trace le haut et le bas de la ligne et retourne sa hauteur *)
let make_line dst y =
  let (w,h) = get_dims dst in
  let rouge = (255, 0, 0) in
  let pixel = ref (0,0,0) in
  let yTemp = ref 0 in
  let compt = ref w in
    for x=0 to w-1 do
      Sdlvideo.put_pixel_color dst x (y-1) rouge;
    done;
    yTemp := y;
    while !compt > 0 do
      compt := 0;
      for x=0 to w-1 do
        pixel := Sdlvideo.get_pixel_color dst x !yTemp;
        if !pixel <= (150,150,150) then (****************** A MODIFIER MAUVAISE COULEUR *****************(=(0,0,0))*)
          compt := !compt + 1
      done;
      yTemp := !yTemp + 1;
    done;
    for x=0 to w-1 do
      Sdlvideo.put_pixel_color dst x (!yTemp-1) rouge
    done;
    !yTemp-y

(* Parcours la ligne donnée en paramètre et enleve les traits rouges inutiles*)
let clear_line img ymin ymax =
  let (w,h) = get_dims img in
  let pixel = ref (0,0,0) in
  let white = ref true in
  let inCar = ref 0 in
  let trans = ref false in
  for x=0 to w-1 do
    white := true;
    for y=ymin to ymax do
      pixel := Sdlvideo.get_pixel_color img x y;
      white := !white && !pixel > (150,150,150); (*************************** A MODIFIER MAUVAISE COULEUR ***************************(0,0,0)*)
    done;
    if !white then
      begin
        if !inCar <> 0 then
          trans := true;
        inCar := 0;
        Sdlvideo.put_pixel_color img x ymin (255,255,255);
        Sdlvideo.put_pixel_color img x ymax (255,255,255);
      end
    else
      inCar := !inCar + 1;
    if !trans then
      begin
        for y=ymin to ymax do
          Sdlvideo.put_pixel_color img x y (255,0,0);
        done;
        trans := false;
      end;
    if !inCar = 1 then
      for y=ymin to ymax do
        Sdlvideo.put_pixel_color img (x-1) y (255,0,0);
      done;
  done;
  ()

(* Parcours profondeur de l'image *)
let parcour_image img =
  let (w,h) = get_dims img in
  let compt = ref 0 in
  let tempo = ref 0 in
  let ligne = ref false in
  let pixel = ref (0,0,0) in
  let ymin = ref 0 in
    for y=0 to h-1 do
      for x=0 to w-1 do
        pixel := Sdlvideo.get_pixel_color img x y;
        Sdlvideo.put_pixel_color img x y !pixel;
      done;
    done;
    for y=0 to h-1 do
      compt := 0;
      if not(!ligne) then
        begin
          for x=0 to w-1 do
            pixel := Sdlvideo.get_pixel_color img x y;
            if !pixel <= (150,150,150) then (********************MAUVAISE VALEURS METTRE "=(0,0,0)"********************)
              compt := !compt + 1;
          done;
          if !compt > 0 then
            begin
              tempo := make_line img y;
              ligne := true
            end
        end
      else
        begin
          if (!tempo <= 0) then
            ligne := false
          else
            tempo := !tempo - 1
        end
    done;
    ligne := false;
    for y=0 to h-1 do
      pixel := Sdlvideo.get_pixel_color img 0 y;
      if !pixel = (255,0,0) then
        begin
          if !ligne then
            begin
              ligne := false;
              clear_line img !ymin y;
            end
          else
            begin
              ymin := y;
              ligne := true;
            end
        end
    done;
    ()
(*
let main () =
  begin 
    (* Nous voulons 1 argument *)
    if Array.length (Sys.argv) < 2 then
      failwith "Il manque le nom du fichier!";
    (* Initialisation de SDL *)
    sdl_init ();
    (* Chargement d'une image *)
    let img = Sdlloader.load_image Sys.argv.(1) in
    (* On récupère les dimensions *)
    let (w,h) = get_dims img in
    (* On crée la surface d'affichage en doublebuffering *)
    let display = Sdlvideo.set_video_mode w h [`DOUBLEBUF] in
      (* on affiche l'image *)
  show img display;
  wait_key ();
  parcour_image img; 
  show img display;
      (* on attend une touche *)
      wait_key ();
      (* on quitte *)
      exit 0
  end
 
let _ = main ()*)
