BATTERIES=`ocamlfind query batteries`
LANGDIR=../Abstract
ERRORDIR=../Error
XMLDIR=`ocamlfind query xml-light`
DOTDIR=../backend
OPTDIR=../Optimization
LANGLIB=language.cmxa
DOTLIB=codegen.cmxa
ERRORLIB=error.cmxa
BATLIBBOPT=batteries.cmxa
OPTIMIZATIONS=optimizations.cmxa
OCMLOPTS=-a
CC=ocamlopt

all: veclang.cmxa

veclang.cmxa: check
	$(CC) $(OCMLOPTS) vectorization.cmx loopCollapse.cmx loopInterchange.cmx loop_out.cmx loopBlock.cmx vecCFG.cmx -o $@

check:
	$(CC) -pp "camlp4o pa_macro.cmo -UDEBUG" -I $(BATTERIES) $(BATLIBBOPT) \
	-I $(LANGDIR) -I $(DOTDIR) -I $(ERRORDIR) -I $(OPTDIR) $(OPTIMIZATIONS) $(ERRORLIB) $(LANGLIB) $(DOTLIB) -c vectorization.ml
	$(CC) -pp "camlp4o pa_macro.cmo -UDEBUG" -I $(BATTERIES) $(BATLIBBOPT) \
	-I $(LANGDIR) -I $(DOTDIR) -I $(ERRORDIR) -I $(OPTDIR) $(OPTIMIZATIONS) $(ERRORLIB) $(LANGLIB) $(DOTLIB) -c loopCollapse.ml
	$(CC) -pp "camlp4o pa_macro.cmo -UDEBUG" -I $(BATTERIES) $(BATLIBBOPT) \
	-I $(LANGDIR) -I $(DOTDIR) -I $(ERRORDIR) -I $(OPTDIR) $(OPTIMIZATIONS) $(ERRORLIB) $(LANGLIB) $(DOTLIB) -c loopInterchange.ml
	$(CC) -pp "camlp4o pa_macro.cmo -DDEBUG" -I $(BATTERIES) $(BATLIBBOPT) \
	-I $(LANGDIR) -I $(DOTDIR) -I $(ERRORDIR) -I $(OPTDIR) $(OPTIMIZATIONS) $(ERRORLIB) $(LANGLIB) $(DOTLIB) -c loop_out.ml
	$(CC) -pp "camlp4o pa_macro.cmo -DDEBUG" -I $(BATTERIES) $(BATLIBBOPT) \
	-I $(LANGDIR) -I $(DOTDIR) -I $(ERRORDIR) -I $(OPTDIR) $(OPTIMIZATIONS) $(ERRORLIB) $(LANGLIB) $(DOTLIB) -c loopBlock.ml
	$(CC) -pp "camlp4o pa_macro.cmo -UDEBUG" -I $(BATTERIES) $(BATLIBBOPT) \
	-I $(LANGDIR) -I $(DOTDIR) -I $(ERRORDIR) $(ERRORLIB) $(LANGLIB) $(DOTLIB) -c vecCFG.ml

clean:
	rm -rf *.cm* *.o *.a
