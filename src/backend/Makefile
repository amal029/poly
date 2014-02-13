COMP_HOME=..
LANGDIR=../Abstract
BATTERIES=`ocamlfind query batteries`
VIPRDIR=$(COMP_HOME)/VIPR
TYPEINFERENCEDIR=$(COMP_HOME)/Checking
XMLDIR=`ocamlfind query xml-light`
ODOTDIR=$(COMP_HOME)/lib
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
