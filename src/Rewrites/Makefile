COMP_HOME=..
LANGDIR=$(COMP_HOME)/Abstract
VIPRDIR=$(COMP_HOME)/VIPR
BATTERIES=`ocamlfind query batteries`
ERRORDIR=$(COMP_HOME)/Error
XMLDIR=`ocamlfind query xml-light`
DOTDIR=$(COMP_HOME)/backend
METISDIR=$(HOME)/IBM_Work/START_WORK_IRELAND/lib-metis/lib
DOTLIB=codegen.cmxa
ERRORLIB=error.cmxa
CC=ocamlopt
XMLLIB=xml-light.cmxa
LANGLIB=language.cmxa
METISLIB=lib-metis.cmxa
BATLIBB=batteries.cmxa

all: fcfg decompile


fcfg: 
	$(CC) -a *.cmx -o rewrites.cmxa
fcheck: 
	$(CC) -I $(LANGDIR) -I $(ERRORDIR) -c fcfg.ml
	$(CC) -I $(LANGDIR) -I $(ERRORDIR) -c cfg.ml

decompile:
	$(CC) -pp "camlp4o pa_macro.cmo -UDEBUG" \
	-I $(DOTDIR) $(DOTLIB) -I $(LANGDIR) -I $(ERRORDIR) -c decompiletoAST.ml
	$(CC) -pp "camlp4o pa_macro.cmo -UDEBUG" \
	-I $(DOTDIR) $(DOTLIB) -I $(LANGDIR) -I $(ERRORDIR) -c decompiletoAndrewLang.ml
	$(CC) -pp "camlp4o pa_macro.cmo -UDEBUG" -I $(DOTDIR) $(DOTLIB)	\
	-I $(METISDIR) $(METISLIB) -I $(LANGDIR) -I $(ERRORDIR) -I $(BATTERIES) $(BATLIBB) -c myStream.ml
	$(CC) -pp "camlp4o pa_macro.cmo -UDEBUG" -I $(DOTDIR) $(DOTLIB)	\
	-I $(LANGDIR) -I $(ERRORDIR) -I $(VIPRDIR) -I $(BATTERIES) -c decompiletoVipr.ml

clean:
	rm -rf *.cm* *.o *.a
