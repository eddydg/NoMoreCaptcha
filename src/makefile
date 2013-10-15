# TP sdl
 
OCAML=ocamlopt
OCAMLFLAGS= -I +sdl -I +site-lib/sdl
OCAMLLD= bigarray.cmxa sdl.cmxa sdlloader.cmxa
OCAMLLD2= fonctions.cmx
 
main: main.ml
	${OCAML} ${OCAMLFLAGS} ${OCAMLLD} ${OCAMLLD2} -o main main.ml

fonctions: fonctions.ml
	${OCAML} ${OCAMLFLAGS} ${OCAMLLD} -o fonctions fonctions.ml
 
clean::
	rm -f *~ *.o *.cm? main && rm -f *~ *.o *.cm? fonction
 
# FIN
