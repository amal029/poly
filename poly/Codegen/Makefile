COMP_HOME=$(HOME)/Dropbox/IBM_Work/START_WORK_IRELAND/std-lang/Ocaml
XMLDIR=$(HOME)/Dropbox/Personal/OCAML_WORK/xml-light
LLVMDIR=/macports-1.9.1/libexec/llvm-3.2/lib/ocaml/
DOTDIR=$(COMP_HOME)/backend
ERRORDIR=$(HOME)/Dropbox/IBM_Work/START_WORK_IRELAND/std-lang/Ocaml/Error
LANGDIR=$(COMP_HOME)/Abstract
XMLLIB=xml-light.cma
LANGLIB=language.cma
LLVMLIB=llvm.cma llvm_executionengine.cma llvm_bitreader.cma
ERRORLIB=error.cma
DOTLIB=codegen.cma
OCMLOPTS=-a
OCMALDEBUG=-g
SRC=andrewxml.ml myLlvm.ml
CC=ocamlc

all: backend.cma


backend.cma:
	$(CC) -pp "camlp4o pa_macro.cmo -UDEBUG" -I $(LANGDIR) -I	\
	   $(XMLDIR) -I $(DOTDIR) -I $(LLVMDIR) -I $(ERRORDIR)		\
	   $(ERRORLIB) $(DOTLIB) $(XMLLIB) $(LANGLIB) $(LLVMLIB)	\
	  $(OCAMLDEBUG) $(OCMLOPTS) $(SRC) -o $@

check: 
	$(CC) -pp "camlp4o pa_macro.cmo -UDEBUG" -I $(LANGDIR) -I	\
	   $(XMLDIR) -I $(DOTDIR) -I $(LLVMDIR) -I $(ERRORDIR)		\
	   $(DOTLIB) $(ERRORLIB) $(XMLLIB) $(LANGLIB) $(LLVMLIB)	\
	  $(OCAMLDEBUG) -i $(SRC)

clean:
	rm -rf *cm* *o *a *mli
