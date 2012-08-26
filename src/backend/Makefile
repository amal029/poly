LANGDIR=$(HOME)/Dropbox/IBM_Work/START_WORK_IRELAND/poly/src/Abstract
TYPEINFERENCEDIR=$(HOME)/Dropbox/IBM_Work/START_WORK_IRELAND/poly/src/Checking
XMLDIR=$(HOME)/Dropbox/Personal/OCAML_WORK/xml-light
ODOTDIR=$(HOME)/Dropbox/IBM_Work/START_WORK_IRELAND/poly/src/lib
FIND=ocamlfind
CC=ocamlopt
XMLLIB=xml-light.cmxa
LANGLIB=language.cmxa
TYPEINFERENCELIB=static-analysis.cmxa
ODOTLIB=odot.cmxa

all: build


build: buildodot
	$(CC) -a *.cmx -o codegen.cmxa

buildodot:
	# $(CC) -I $(LANGDIR) -I $(ODOTDIR) -I $(TYPEINFERENCEDIR) -i dot.ml
	$(CC) -I $(LANGDIR) -I $(ODOTDIR) -c dot.ml
	$(CC) -pp "camlp4o pa_macro.cmo -UDEBUG" \
	-I $(LANGDIR) -I $(ODOTDIR) -c stream_dot.ml

clean:
	rm -rf *.cm* *mli *.a *.o
