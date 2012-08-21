LANGDIR=$(HOME)/Dropbox/IBM_Work/START_WORK_IRELAND/poly/src/Abstract
ERRORDIR=$(HOME)/Dropbox/IBM_Work/START_WORK_IRELAND/poly/src/Error
XMLDIR=$(HOME)/Dropbox/Personal/OCAML_WORK/xml-light
DOTDIR=$(HOME)/Dropbox/IBM_Work/START_WORK_IRELAND/poly/src/backend
METISDIR=$(HOME)/Dropbox/IBM_Work/START_WORK_IRELAND/lib-metis/lib
DOTLIB=codegen.cma
ERRORLIB=error.cma
CC=ocamlc
XMLLIB=xml-light.cma
LANGLIB=language.cma
METISLIB=lib-metis.cma

all: fcfg decompile


fcfg: 
	$(CC) -I $(LANGDIR) -I $(ERRORDIR) $(ERRORLIB) -a *.cmo -o rewrites.cma
fcheck: 
	$(CC) -I $(LANGDIR) -I $(ERRORDIR) -c fcfg.ml
	$(CC) -I $(LANGDIR) -I $(ERRORDIR) -c cfg.ml

decompile:
	$(CC) -pp "camlp4o pa_macro.cmo -UDEBUG" \
	-I $(DOTDIR) $(DOTLIB) -I $(LANGDIR) -I $(ERRORDIR) -c decompiletoAST.ml
	$(CC) -pp "camlp4o pa_macro.cmo -UDEBUG" \
	-I $(DOTDIR) $(DOTLIB) -I $(LANGDIR) -I $(ERRORDIR) -c decompiletoAndrewLang.ml
	$(CC) -pp "camlp4o pa_macro.cmo -UDEBUG" -I $(DOTDIR) $(DOTLIB)	\
	-I $(METISDIR) $(METISLIB) -I $(LANGDIR) -I $(ERRORDIR) -c myStream.ml

clean:
	rm -rf *.cm*
