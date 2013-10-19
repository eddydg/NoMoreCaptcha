# NoMoreCaptcha - OCR project

OCAML= ocamlopt
OCAMLFLAGS= -I +sdl -I +site-lib/sdl
OCAMLLD= bigarray.cmxa sdl.cmxa sdlloader.cmxa
main: main.ml
	${OCAML} ${OCAMLFLAGS} ${OCAMLLD} -o NoMoreCaptcha conv_matrix.ml image_tools.ml fonctions.ml neural_network.ml rotation.ml main.ml

clean::
	rm -f *~ *.cm? *.o NoMoreCaptcha

#FIN
