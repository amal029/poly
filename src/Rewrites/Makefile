COMP_HOME=$(ROOTDIR)
LANGDIR=$(ROOTDIR)/Abstract
VIPRDIR=$(COMP_HOME)/VIPR
ERRORDIR=$(ROOTDIR)/Error
DOTDIR=$(ROOTDIR)/backend
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
