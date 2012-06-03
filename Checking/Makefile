LANGDIR=$(HOME)/Dropbox/IBM_Work/START_WORK_IRELAND/std-lang/Ocaml/Abstract
BACKDIR=$(HOME)/Dropbox/IBM_Work/START_WORK_IRELAND/std-lang/Ocaml/backend
ERRORDIR=$(HOME)/Dropbox/IBM_Work/START_WORK_IRELAND/std-lang/Ocaml/Error
PARSEDIR=$(HOME)/Dropbox/IBM_Work/START_WORK_IRELAND/std-lang/Ocaml/Parsing
REWRITEDIR=$(HOME)/Dropbox/IBM_Work/START_WORK_IRELAND/std-lang/Ocaml/Optimization
XMLDIR=$(HOME)/Dropbox/Personal/OCAML_WORK/xml-light
CC=ocamlc
XMLLIB=xml-light.cma
LANGLIB=language.cma
CODEGENLIB=codegen.cma
REWRITELIB=optimizations.cma
PARSERLIB=parser.cma 
ERRORLIB=error.cma


all: pack

pack: check
	$(CC) -I $(LANGDIR) -I $(BACKDIR) -I $(REWRITEDIR) $(CODEGENLIB) $(LANGLIB) \
	$(REWRITELIB) -I $(ERRORDIR) $(ERRORLIB) -a *.cmo -o static-analysis.cma
check: 
	$(CC) -I $(LANGDIR) -I $(PARSEDIR) $(PARSELIB) $(LANGLIB) -c syntax_check.ml
	$(CC) -pp "camlp4o pa_macro.cmo -DDEBUG" -I $(LANGDIR) -I	\
	$(BACKDIR) -I $(REWRITEDIR) -I $(PARSEDIR) $(PARSELIB)		\
	$(LANGLIB) -I $(ERRORDIR) $(ERRORLIB) -c type_inference.ml

clean:
	rm -rf *.cm* *mli

