# BATTERIES=/macports-1.9.1/lib/ocaml/site-lib/batteries/
LANGDIR=$(ROOTDIR)/Abstract
ERRORDIR=$(ROOTDIR)/Error
DOTDIR=$(ROOTDIR)/backend
FIND=ocamlfind
CC=ocamlopt
DOTLIB=codegen.cmxa
XMLLIB=xml-light.cmxa
LANGLIB=language.cmxa
BATLIB=batteries_uni.cmxa
BATLIBB=batteries.cmxa
CAMOMILELIB=camomile.cmxa
CAMOMILELIBB=camomileLibrary.cmxa
BIGARRAYLIB=bigarray.cmxa
NUMSLIB=nums.cmxa
UNIXLIB=unix.cmxa
STRLIB=str.cmxa
ERRORLIB=error.cmxa

all: optimization


optimization: fcheck
	$(CC) -a constantfolding.cmx -o optimizations.cmxa

fcheck:
	$(CC) -pp "camlp4o pa_macro.cmo -UDEBUG" -I $(ERRORDIR) -I	\
	$(LANGDIR) $(LANGLIB) -I $(BATTERIES) -I $(DOTDIR) $(ERRORLIB)	\
	$(DOTLIB) -c constantfolding.ml

clean:
	rm -rf *.cm* *mli *.a *.o
