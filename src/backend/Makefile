LANGDIR=$(HOME)/Dropbox/IBM_Work/START_WORK_IRELAND/poly/src/Abstract
TYPEINFERENCEDIR=$(HOME)/Dropbox/IBM_Work/START_WORK_IRELAND/poly/src/Checking
XMLDIR=$(HOME)/Dropbox/Personal/OCAML_WORK/xml-light
ODOTDIR=$(HOME)/Dropbox/IBM_Work/START_WORK_IRELAND/poly/src/lib
FIND=ocamlfind
CC=ocamlc
XMLLIB=xml-light.cma
LANGLIB=language.cma
TYPEINFERENCELIB=static-analysis.cma
ODOTLIB=odot.cma

all: build


build: buildodot
	$(CC) -I $(LANGDIR) -I $(ODOTDIR) $(ODOTLIB) -a *.cmo -o	\
	codegen.cma

buildodot:
	# $(CC) -I $(LANGDIR) -I $(ODOTDIR) -I $(TYPEINFERENCEDIR) -i dot.ml
	$(CC) -I $(LANGDIR) -I $(ODOTDIR) -c dot.ml
	$(CC) -pp "camlp4o pa_macro.cmo -UDEBUG" \
	-I $(LANGDIR) -I $(ODOTDIR) -c stream_dot.ml

clean:
	rm -rf *.cm* *mli
