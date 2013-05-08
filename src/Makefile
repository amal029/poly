# The toplevel makefile, which calls all the required makefiles one
# after the other

export ROOTDIR = $(PWD)
export XMLDIR = /home/andrew/PhD/tools/POLY/xml-light
export ODOTDIR = /home/andrew/PhD/tools/POLY/odot/odot-0.6
export METISDIR = /home/andrew/PhD/tools/POLY/lib-metis/lib
export BATTERIES = /usr/lib/ocaml/batteries
export CAMOMILEDIR = /usr/lib/ocaml/camomile
export BIGARRAYDIR= /usr/lib/ocaml
export LLVMDIR = /opt/llvm/lib/ocaml

all: compile

compile:
	$(MAKE) -e -C Abstract/ all
	$(MAKE) -e -C Error/ all
	$(MAKE) -e -C Parsing/ all
	$(MAKE) -e -C backend/ all
	$(MAKE) -e -C VIPR/ all
	$(MAKE) -e -C backend/ viprout
	$(MAKE) -e -C Rewrites/ fcheck
	$(MAKE) -e -C Rewrites/ decompile
	$(MAKE) -e -C Rewrites/ fcfg
	$(MAKE) -e -C Optimization/ all
	$(MAKE) -e -C Checking/ all
	$(MAKE) -e -C Vectorization/ all
	$(MAKE) -e -C Codegen/ all
	$(MAKE) -e -C Tests/ all
	ctags -R -e *
	mkdir -p output
	mkdir -p output1

clean:
	$(MAKE) -e -C Abstract/ clean
	$(MAKE) -e -C Error/ clean
	$(MAKE) -e -C Parsing/ clean
	$(MAKE) -e -C VIPR/ clean
	$(MAKE) -e -C Checking/ clean
	$(MAKE) -e -C Rewrites/ clean
	$(MAKE) -e -C Optimization/ clean
	$(MAKE) -e -C backend/ clean
	$(MAKE) -e -C Codegen/ clean
	$(MAKE) -e -C Vectorization/ clean
	$(MAKE) -e -C Tests/ clean
	rm -rf *.ll *.lle *.bc *.s *.dot *.grf *.part* gmon.out TAGS

run:
	$(MAKE) -e -C Tests/ run
