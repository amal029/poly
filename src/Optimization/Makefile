# BATTERIES=/macports-1.9.1/lib/ocaml/site-lib/batteries/
LANGDIR=$(HOME)/Dropbox/IBM_Work/START_WORK_IRELAND/poly/src/Abstract
ERRORDIR=$(HOME)/Dropbox/IBM_Work/START_WORK_IRELAND/poly/src/Error
DOTDIR=$(HOME)/Dropbox/IBM_Work/START_WORK_IRELAND/poly/src/backend
XMLDIR=$(HOME)/Dropbox/Personal/OCAML_WORK/xml-light
BATTERIES=$(HOME)/batteries-included/_build/src
CAMOMILEDIR=/macports-1.9.1/lib/ocaml/site-lib/camomile
BIGARRAYDIR=/macports-1.9.1/lib/ocaml/
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
