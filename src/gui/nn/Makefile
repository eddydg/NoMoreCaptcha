OCAML= ocamlopt
OCAMLFLAGS= -I +sdl -I +site-lib/sdl
OCAMLLD= bigarray.cmxa sdl.cmxa sdlloader.cmxa sdlttf.cmxa
SOURCE= tools.ml neuron.ml network.ml nn.ml
MLI= ${SOURCE:.ml = .mli}
main: 
	${OCAML} ${OCAMLFLAGS} ${OCAMLLD} -o out ${SOURCE} 

clean::
	rm -f *~ *.cm? *.o out
	
