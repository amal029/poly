BATTERIES=$(HOME)/batteries-included/_build/src
LANGDIR=$(HOME)/Dropbox/IBM_Work/START_WORK_IRELAND/poly/src/Abstract
ERRORDIR=$(HOME)/Dropbox/IBM_Work/START_WORK_IRELAND/poly/src/Error
XMLDIR=$(HOME)/Dropbox/Personal/OCAML_WORK/xml-light
DOTDIR=$(HOME)/Dropbox/IBM_Work/START_WORK_IRELAND/poly/src/backend
LANGLIB=language.cmxa
DOTLIB=codegen.cmxa
ERRORLIB=error.cmxa
BATLIBBOPT=batteries.cmxa
OCMLOPTS=-a
CC=ocamlopt

all: veclang.cmxa

veclang.cmxa: check
	$(CC) $(OCMLOPTS) vectorization.cmx vecCFG.cmx -o $@

check:
	$(CC) -pp "camlp4o pa_macro.cmo -UDEBUG" -I $(BATTERIES) $(BATLIBBOPT) \
	-I $(LANGDIR) -I $(DOTDIR) -I $(ERRORDIR) $(ERRORLIB) $(LANGLIB) $(DOTLIB) -c vectorization.ml
	$(CC) -pp "camlp4o pa_macro.cmo -UDEBUG" -I $(BATTERIES) $(BATLIBBOPT) \
	-I $(LANGDIR) -I $(DOTDIR) -I $(ERRORDIR) $(ERRORLIB) $(LANGLIB) $(DOTLIB) -c vecCFG.ml

clean:
	rm -rf *.cm* *.o *.a
