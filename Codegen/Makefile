COMP_HOME=$(HOME)/Dropbox/IBM_Work/START_WORK_IRELAND/std-lang/Ocaml
XMLDIR=$(HOME)/Dropbox/Personal/OCAML_WORK/xml-light
LLVMDIR=/macports-1.9.1/libexec/llvm-3.2/lib/ocaml/
ERRORDIR=$(HOME)/Dropbox/IBM_Work/START_WORK_IRELAND/std-lang/Ocaml/Error
LANGDIR=$(COMP_HOME)/Abstract
XMLLIB=xml-light.cma
LANGLIB=language.cma
LLVMLIB=llvm.cma
ERRORLIB=error.cma
OCMLOPTS=-a
OCMALDEBUG=-g
SRC=andrewxml.ml myLlvm.ml
CC=ocamlc

all: backend.cma


backend.cma: check
	$(CC) -pp "camlp4o pa_macro.cmo -DDEBUG" -I $(LANGDIR) -I	\
	 $(XMLDIR) -I $(LLVMDIR) -I $(ERRORDIR) $(ERRORLIB) $(XMLLIB)	\
	 $(LANGLIB) $(LLVMLIB) $(OCAMLDEBUG) $(OCMLOPTS) $(SRC) -o $@

check: 
	$(CC) -pp "camlp4o pa_macro.cmo -DDEBUG" -I $(LANGDIR) -I	\
	 $(XMLDIR) -I $(LLVMDIR) -I $(ERRORDIR) $(ERRORLIB) $(XMLLIB)	\
	 $(LANGLIB) $(LLVMLIB) $(OCAMLDEBUG) -i $(SRC) 

clean:
	rm -rf *cm* *o *a *mli
