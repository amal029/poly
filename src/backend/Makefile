COMP_HOME=$(HOME)/Dropbox/IBM_Work/START_WORK_IRELAND/poly/src
LANGDIR=$(HOME)/Dropbox/IBM_Work/START_WORK_IRELAND/poly/src/Abstract
BATTERIES=$(HOME)/batteries-included/_build/src
VIPRDIR=$(COMP_HOME)/VIPR
TYPEINFERENCEDIR=$(HOME)/Dropbox/IBM_Work/START_WORK_IRELAND/poly/src/Checking
XMLDIR=$(HOME)/Dropbox/Personal/OCAML_WORK/xml-light
ODOTDIR=$(HOME)/Dropbox/IBM_Work/START_WORK_IRELAND/poly/src/lib
FIND=ocamlfind
CC=ocamlopt
XMLLIB=xml-light.cmxa
LANGLIB=language.cmxa
TYPEINFERENCELIB=static-analysis.cmxa
ODOTLIB=odot.cmxa
VIPRLIB=vipr.cmxa

all: build


build: buildodot

buildodot:
	# $(CC) -I $(LANGDIR) -I $(ODOTDIR) -I $(TYPEINFERENCEDIR) -i dot.ml
	$(CC) -I $(LANGDIR) -I $(ODOTDIR) -c dot.ml
	$(CC) -pp "camlp4o pa_macro.cmo -UDEBUG" -I $(LANGDIR) -I $(ODOTDIR) -c stream_dot.ml

viprout:
	$(CC) -pp "camlp4o pa_macro.cmo -UDEBUG" -I $(LANGDIR) -I $(ODOTDIR) \
	-I $(VIPRDIR) -I $(BATTERIES) -c viprout.ml
	$(CC) -a stream_dot.cmx dot.cmx viprout.cmx -o codegen.cmxa

clean:
	rm -rf *.cm* *mli *.a *.o
