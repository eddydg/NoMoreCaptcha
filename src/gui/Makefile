# NoMoreCaptcha - OCR project

OCAML= ocamlopt
OCAMLFLAGS= -w s -I +sdl -I +site-lib/sdl -I +lablgtk2
OCAMLLD= bigarray.cmxa sdl.cmxa sdlloader.cmxa lablgtk.cmxa lablgtkspell.cmxa sdlttf.cmxa
main: gui.ml
	${OCAML} ${OCAMLFLAGS} ${OCAMLLD} -o Gui image_tools.ml detect.ml fonctions.ml rotation.ml characters.ml tools.ml neuron.ml network.ml nn.ml gui.ml

clean::
	rm -f *~ *.cm? *.o output.bmp rotated.bmp Gui

#FIN

