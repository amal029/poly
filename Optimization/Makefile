LANGDIR=$(HOME)/Dropbox/IBM_Work/START_WORK_IRELAND/std-lang/Ocaml/Abstract
DOTDIR=$(HOME)/Dropbox/IBM_Work/START_WORK_IRELAND/std-lang/Ocaml/backend
XMLDIR=$(HOME)/Dropbox/Personal/OCAML_WORK/xml-light
BATTERIES=/macports-1.9.1/lib/ocaml/site-lib/batteries/
CAMOMILEDIR=/macports-1.9.1/lib/ocaml/site-lib/camomile
BIGARRAYDIR=/macports-1.9.1/lib/ocaml/
FIND=ocamlfind
CC=ocamlc
DOTLIB=codegen.cma
XMLLIB=xml-light.cma
LANGLIB=language.cma
BATLIB=batteries_uni.cma
BATLIBB=batteries.cma
CAMOMILELIB=camomile.cma
CAMOMILELIBB=camomileLibrary.cma
BIGARRAYLIB=bigarray.cma
NUMSLIB=nums.cma
UNIXLIB=unix.cma
STRLIB=str.cma

all: optimization


optimization: fcheck
	$(CC) -pp "camlp4o pa_macro.cmo -DDEBUG" -I $(LANGDIR)		\
	            $(BIGARRAYLIB) $(NUMSLIB) $(UNIXLIB) $(STRLIB) -I	\
	            $(CAMOMILEDIR) $(CAMOMILELIBB) $(CAMOMILELIB)	\
	            $(NUMSLIB) $(BIGARRAYLIB) -I $(BATTERIES)		\
	            $(BATLIBB) $(BATLIB) -I $(DOTDIR) $(DOTLIB) -a	\
	            *.cmo -o optimizations.cma
fcheck: 
	# $(CC) -pp "camlp4o pa_macro.cmo -UDEBUG" -I $(LANGDIR) -I $(BATTERIES) -I $(DOTDIR) $(DOTLIB) -i constantfolding.ml
	$(CC) -pp "camlp4o pa_macro.cmo -UDEBUG" -I $(LANGDIR) $(LANGLIB) -I $(BATTERIES) -I $(DOTDIR) $(DOTLIB) -c constantfolding.ml

clean:
	rm -rf *.cm* *mli
