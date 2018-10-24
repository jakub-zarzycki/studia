test: arytmetyka
	./arytmetyka
	make clean

arytmetyka: arytmetyka.cmo
	ocamlc -o $@ $^

%.cmo: %.ml
	ocamlc -c $<

%.cmi: %.mli
	ocamlc -c $<
clean:
	rm *.cmi *.cmo

depend:
	ocamldep *.ml *.mli > .depend

include .depend
