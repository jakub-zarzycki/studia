test: arytmetyka.cmx
	ocamlopt -o test arytmetyka.cmx testy/test.ml 
	./test
	ocamlopt -o test1 arytmetyka.cmx testy/test01.ml
	./test1
	ocamlopt -o test2 arytmetyka.cmx testy/test02.ml
	./test2
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
