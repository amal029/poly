BATTERIES=$(HOME)/batteries-included/_build/src
LANGDIR=$(HOME)/Dropbox/IBM_Work/START_WORK_IRELAND/poly/src/Abstract
ERRORDIR=$(HOME)/Dropbox/IBM_Work/START_WORK_IRELAND/poly/src/Error
XMLDIR=$(HOME)/Dropbox/Personal/OCAML_WORK/xml-light
DOTDIR=$(HOME)/Dropbox/IBM_Work/START_WORK_IRELAND/poly/src/backend
OPTDIR=$(HOME)/Dropbox/IBM_Work/START_WORK_IRELAND/poly/src/Optimization
LANGLIB=language.cmxa
DOTLIB=codegen.cmxa
ERRORLIB=error.cmxa
BATLIBBOPT=batteries.cmxa
OPTIMIZATIONS=optimizations.cmxa
OCMLOPTS=-a
CC=ocamlopt

all: veclang.cmxa

veclang.cmxa: check
	$(CC) $(OCMLOPTS) vectorization.cmx loopCollapse.cmx loopInterchange.cmx vecCFG.cmx -o $@

check:
	$(CC) -pp "camlp4o pa_macro.cmo -UDEBUG" -I $(BATTERIES) $(BATLIBBOPT) \
	-I $(LANGDIR) -I $(DOTDIR) -I $(ERRORDIR) -I $(OPTDIR) $(OPTIMIZATIONS) $(ERRORLIB) $(LANGLIB) $(DOTLIB) -c vectorization.ml
	$(CC) -pp "camlp4o pa_macro.cmo -UDEBUG" -I $(BATTERIES) $(BATLIBBOPT) \
	-I $(LANGDIR) -I $(DOTDIR) -I $(ERRORDIR) -I $(OPTDIR) $(OPTIMIZATIONS) $(ERRORLIB) $(LANGLIB) $(DOTLIB) -c loopCollapse.ml
	$(CC) -pp "camlp4o pa_macro.cmo -UDEBUG" -I $(BATTERIES) $(BATLIBBOPT) \
	-I $(LANGDIR) -I $(DOTDIR) -I $(ERRORDIR) -I $(OPTDIR) $(OPTIMIZATIONS) $(ERRORLIB) $(LANGLIB) $(DOTLIB) -c loopInterchange.ml
	$(CC) -pp "camlp4o pa_macro.cmo -UDEBUG" -I $(BATTERIES) $(BATLIBBOPT) \
	-I $(LANGDIR) -I $(DOTDIR) -I $(ERRORDIR) $(ERRORLIB) $(LANGLIB) $(DOTLIB) -c vecCFG.ml

clean:
	rm -rf *.cm* *.o *.a
