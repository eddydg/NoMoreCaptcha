# NoMoreCaptcha - OCR project

OCAML= ocamlopt
OCAMLFLAGS= -w s -I +sdl -I +site-lib/sdl -I +lablgtk2
OCAMLLD= bigarray.cmxa sdl.cmxa sdlloader.cmxa lablgtk.cmxa lablgtkspell.cmxa
main: gui.ml
	${OCAML} ${OCAMLFLAGS} ${OCAMLLD} -o Gui image_tools.ml detect.ml fonctions.ml rotation.ml gui.ml

clean::
	rm -f *~ *.cm? *.o output.bmp rotated.bmp Gui

#FIN

