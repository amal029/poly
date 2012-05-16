LANGDIR=$(HOME)/Dropbox/IBM_Work/START_WORK_IRELAND/std-lang/Ocaml/Abstract
XMLDIR=$(HOME)/Dropbox/Personal/OCAML_WORK/xml-light
FIND=ocamlfind
CC=ocamlc
XMLLIB=xml-light.cma
LANGLIB=language.cma

all: optimization


optimization: fcheck
	$(FIND) $(CC) -I $(LANGDIR) $(LANGLIB) -package batteries -a *.cmo -o optimizations.cma
fcheck: 
	$(FIND) $(CC) -I $(LANGDIR) -package batteries -i constant_propogation.ml
	$(FIND) $(CC) -I $(LANGDIR) $(LANGLIB) -package batteries -c constant_propogation.ml
	$(FIND) $(CC) -I $(LANGDIR) -package batteries -i constant_folding.ml
	$(FIND) $(CC) -I $(LANGDIR) $(LANGLIB) -package batteries -c constant_folding.ml

clean:
	rm -rf *.cm* *mli
