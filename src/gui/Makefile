# NoMoreCaptcha - OCR project

OCAML= ocamlopt
OCAMLFLAGS= -w s -I +sdl -I +site-lib/sdl -I +site-lib/lablgtk2
OCAMLLD= bigarray.cmxa sdl.cmxa sdlloader.cmxa lablgtk.cmxa
main: gui.ml
	${OCAML} ${OCAMLFLAGS} ${OCAMLLD} -o Gui image_tools.ml rotation.ml gui.ml

clean::
	rm -f *~ *.cm? *.o Gui

#FIN

