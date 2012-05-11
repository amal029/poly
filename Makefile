# The toplevel makefile, which calls all the required makefiles one
# after the other


all: compile


compile:
	$(MAKE) -C Abstract/ all
	$(MAKE) -C Parsing/ all
	$(MAKE) -C Rewrites/ all
	$(MAKE) -C Checking/ all
	$(MAKE) -C Optimization/ all
	$(MAKE) -C Tests/ all

clean:
	$(MAKE) -C Abstract/ clean
	$(MAKE) -C Parsing/ clean
	$(MAKE) -C Checking/ clean
	$(MAKE) -C Rewrites/ clean
	$(MAKE) -C Optimization/ clean
	$(MAKE) -C Tests/ clean

run:
	$(MAKE) -C Tests/ run
