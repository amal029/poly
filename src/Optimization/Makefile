# BATTERIES=/macports-1.9.1/lib/ocaml/site-lib/batteries/
LANGDIR=$(HOME)/Dropbox/IBM_Work/START_WORK_IRELAND/poly/src/Abstract
ERRORDIR=$(HOME)/Dropbox/IBM_Work/START_WORK_IRELAND/poly/src/Error
DOTDIR=$(HOME)/Dropbox/IBM_Work/START_WORK_IRELAND/poly/src/backend
XMLDIR=$(HOME)/Dropbox/Personal/OCAML_WORK/xml-light
BATTERIES=$(HOME)/batteries-included/_build/src
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
ERRORLIB=error.cma

all: optimization


optimization: fcheck
	$(CC) -pp "camlp4o pa_macro.cmo -UDEBUG" -I $(LANGDIR)		\
	             $(BIGARRAYLIB) $(NUMSLIB) $(UNIXLIB) $(STRLIB) -I	\
	             $(CAMOMILEDIR) $(CAMOMILELIBB) $(CAMOMILELIB)	\
	             $(NUMSLIB) $(BIGARRAYLIB) -I $(BATTERIES) -I	\
	            $(ERRORDIR) $(BATLIBB) $(ERRORLIB) -I	\
	            $(DOTDIR) $(DOTLIB) -a *.cmo -o optimizations.cma
	# $(CC) -pp "camlp4o pa_macro.cmo -UDEBUG" -I $(LANGDIR)		\
	#             $(BIGARRAYLIB) $(NUMSLIB) $(UNIXLIB) $(STRLIB) -I	\
	#             $(CAMOMILEDIR) $(CAMOMILELIBB) $(CAMOMILELIB)	\
	#             $(NUMSLIB) $(BIGARRAYLIB) \
	#             -I $(DOTDIR) $(DOTLIB) -a	\
	#             *.cmo -o optimizations.cma
fcheck: 
	# $(CC) -pp "camlp4o pa_macro.cmo -UDEBUG" -I $(LANGDIR) -I $(BATTERIES) -I $(DOTDIR) $(DOTLIB) -i constantfolding.ml
	$(CC) -pp "camlp4o pa_macro.cmo -UDEBUG" -I $(ERRORDIR) -I	\
	$(LANGDIR) $(LANGLIB) -I $(BATTERIES) -I $(DOTDIR) $(ERRORLIB)	\
	$(DOTLIB) -c constantfolding.ml
	# $(CC) -pp "camlp4o pa_macro.cmo -UDEBUG" \
	# -I $(ERRORDIR) -I $(LANGDIR) $(LANGLIB) -I $(DOTDIR) $(DOTLIB) -c constantfolding.ml

clean:
	rm -rf *.cm* *mli
