let line_this dst y =
  let (w,h) = Image_tools.get_dim dst in
  let red = (255, 0, 0) in
  let pixel = ref (0,0,0) in
  let yTemp = ref 0 in
  let count = ref w in
    for x=0 to w-1 do
	if Sdlvideo.get_pixel_color dst x (y-1) > (150,150,150) then
      	Sdlvideo.put_pixel_color dst x (y-1) red
    done;
    yTemp := y;
    while !count > 5 do
      count := 0;
      for x=0 to w-1 do
        pixel := Sdlvideo.get_pixel_color dst x !yTemp;
        if !pixel <= (150,150,150) then
          count := !count + 1
      done;
      yTemp := !yTemp + 1;
    done;
    for x=0 to w-1 do
	if Sdlvideo.get_pixel_color dst x (!yTemp-1) > (150,150,150) then
     	  Sdlvideo.put_pixel_color dst x (!yTemp-1) red
    done;
    !yTemp-y

let clear_this img ymin ymax =
  let (w,h) = Image_tools.get_dim img in
  let pixel = ref (0,0,0) in
  let white = ref true in
  let inCar = ref 0 in
  let trans = ref false in
if abs(ymax-ymin) > 5 then
 begin
  for x=0 to w-1 do
    white := true;
    for y=ymin to ymax do
      pixel := Sdlvideo.get_pixel_color img x y;
      white := !white && !pixel > (150,150,150);
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
 end
else
  for x = 0 to w-1 do
	Sdlvideo.put_pixel_color img x ymin (255,255,255);
	Sdlvideo.put_pixel_color img x ymax (255,255,255)
  done;
 ()


let circle_this img =
  let (w,h) = Image_tools.get_dim img in
  let count = ref 0 in
  let tempo = ref 0 in
  let line = ref false in
  let pixel = ref (0,0,0) in
  let ymin = ref 0 in
    for y=0 to h-1 do
      for x=0 to w-1 do
        pixel := Sdlvideo.get_pixel_color img x y;
        Sdlvideo.put_pixel_color img x y !pixel;
      done;
    done;
    for y=0 to h-1 do
      count := 0;
      if not(!line) then
        begin
          for x=0 to w-1 do
            pixel := Sdlvideo.get_pixel_color img x y; 
            if !pixel <= (150,150,150) then
              count := !count + 1;
          done;
          if !count > 5 then
            begin
              tempo := line_this img y;
              line := true
            end
        end
      else
        begin
          if (!tempo <= 0) then
            line := false
          else
            tempo := !tempo - 1
        end
    done;
    line := false;
    for y=0 to h-1 do
      pixel := Sdlvideo.get_pixel_color img 0 y;
      if !pixel = (255,0,0) then
        begin
          if !line then
            begin
              line := false;
              clear_this img !ymin y;
            end
          else
            begin
              ymin := y;
              line := true;
            end
        end
    done;
    ()
