# The base name of the executable
PROG=testy

# The list of object files
CMOS=iSet.cmo testy.cmo
CMXS=$(CMOS:.cmo=.cmx)

# Choose your target: bytecode, native or both
all: $(PROG).byte
#all: $(PROG).native
#all: $(PROG).byte $(PROG).native

OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
OCAMLDOC=ocamldoc
INCLUDES= # all relevant -I options here
OCAMLFLAGS=$(INCLUDES) # add other options for ocamlc here
OCAMLOPTFLAGS=$(INCLUDES) # add other options for ocamlopt here

$(PROG).byte: $(CMOS)
		$(OCAMLC) $(OCAMLFLAGS) -o $@ $^


$(PROG).native: $(CMXS)
		$(OCAMLOPT) $(OCAMLOPTFLAGS) -o $@ $^


.SUFFIXES: .ml .mli .cmo .cmi .cmx

%.cmo: %.ml
		$(OCAMLC) $(OCAMLFLAGS) -c $<

%.cmx: %.ml
		$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $<

%.cmi: %.mli
		$(OCAMLC) $(OCAMLFLAGS) -c $<

.FORCE:

doc: .FORCE
		mkdir -p doc
		$(OCAMLDOC) $(INCLUDES) -html -d doc *.mli *.ml

clean:
		rm -f *.cm[iox]
		rm -f *.o

.depend:
		$(OCAMLDEP) $(INCLUDES) *.mli *.ml > $@

depend:
		rm .depend
		$(MAKE) .depend

include .depend
