# The toplevel makefile, which calls all the required makefiles one
# after the other


all: compile


compile:
	$(MAKE) -C Abstract/ all
	$(MAKE) -C Parsing/ all
	$(MAKE) -C Rewrites/ fcheck
	$(MAKE) -C backend/ all
	$(MAKE) -C Rewrites/ decompile
	$(MAKE) -C Rewrites/ fcfg
	$(MAKE) -C Optimization/ all
	$(MAKE) -C Checking/ all
	$(MAKE) -C Codegen/ all
	$(MAKE) -C Tests/ all
	mkdir -p output
	mkdir -p output1

clean:
	$(MAKE) -C Abstract/ clean
	$(MAKE) -C Parsing/ clean
	$(MAKE) -C Checking/ clean
	$(MAKE) -C Rewrites/ clean
	$(MAKE) -C Optimization/ clean
	$(MAKE) -C backend/ clean
	$(MAKE) -C Codegen/ clean
	$(MAKE) -C Tests/ clean

run:
	$(MAKE) -C Tests/ run
