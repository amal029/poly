LANGDIR=$(HOME)/Dropbox/IBM_Work/START_WORK_IRELAND/std-lang/Ocaml/Abstract
TYPEINFERENCEDIR=$(HOME)/Dropbox/IBM_Work/START_WORK_IRELAND/std-lang/Ocaml/Checking
XMLDIR=$(HOME)/Dropbox/Personal/OCAML_WORK/xml-light
ODOTDIR=$(HOME)/Dropbox/IBM_Work/START_WORK_IRELAND/std-lang/Ocaml/lib
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

clean:
	rm -rf *.cm* *mli
