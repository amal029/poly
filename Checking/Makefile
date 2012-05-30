LANGDIR=$(HOME)/Dropbox/IBM_Work/START_WORK_IRELAND/std-lang/Ocaml/Abstract
BACKDIR=$(HOME)/Dropbox/IBM_Work/START_WORK_IRELAND/std-lang/Ocaml/backend
REWRITEDIR=$(HOME)/Dropbox/IBM_Work/START_WORK_IRELAND/std-lang/Ocaml/Optimization
XMLDIR=$(HOME)/Dropbox/Personal/OCAML_WORK/xml-light
CC=ocamlc
XMLLIB=xml-light.cma
LANGLIB=language.cma
CODEGENLIB=codegen.cma
REWRITELIB=optimizations.cma


all: pack

pack: check
	$(CC) -I $(LANGDIR) -I $(BACKDIR) -I $(REWRITEDIR) $(CODEGENLIB) $(LANGLIB) \
	$(REWRITELIB) -a *.cmo -o static-analysis.cma
check: 
	$(CC) -I $(LANGDIR) $(LANGLIB) -c syntax_check.ml
	$(CC) -pp "camlp4o pa_macro.cmo -UDEBUG" -I $(LANGDIR) -I $(BACKDIR) -I $(REWRITEDIR) \
	$(LANGLIB) -c type_inference.ml

clean:
	rm -rf *.cm* *mli

