LANGDIR=$(HOME)/Dropbox/IBM_Work/START_WORK_IRELAND/std-lang/Ocaml/Abstract
XMLDIR=$(HOME)/Dropbox/Personal/OCAML_WORK/xml-light
DOTDIR=$(HOME)/Dropbox/IBM_Work/START_WORK_IRELAND/std-lang/Ocaml/backend
DOTLIB=codegen.cma
CC=ocamlc
XMLLIB=xml-light.cma
LANGLIB=language.cma

all: fcfg decompile


fcfg: 
	$(CC) -I $(LANGDIR) -a *.cmo -o rewrites.cma
fcheck: 
	$(CC) -I $(LANGDIR) -c fcfg.ml
	$(CC) -I $(LANGDIR) -c cfg.ml

decompile:
	$(CC) -pp "camlp4o pa_macro.cmo -UDEBUG" \
	-I $(DOTDIR) $(DOTLIB) -I $(LANGDIR) -c decompiletoAST.ml
	$(CC) -pp "camlp4o pa_macro.cmo -UDEBUG" \
	-I $(DOTDIR) $(DOTLIB) -I $(LANGDIR) -c decompiletoAndrewLang.ml

clean:
	rm -rf *.cm*
