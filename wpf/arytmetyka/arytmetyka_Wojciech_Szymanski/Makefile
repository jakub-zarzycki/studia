test: test.ml arytmetyka.cmx
	ocamlopt -o test arytmetyka.cmx test.ml 
	./test
	#make clean

arytmetyka: arytmetyka.mli arytmetyka.ml
	ocamlc -o $@ $^

debug: arytmetyka.ml
	ocamlc -g arytmetyka.ml

%.cmo: %.mli %.ml
	ocamlc -c $^

%.cmx: %.ml
	ocamlopt -c $<

%.cmi: %.mli
	ocamlopt -c $<

clean:
	rm *.cmi *.cmo *.cmx *.o

depend:
	ocamldep *.ml *.mli > .depend

include .depend
