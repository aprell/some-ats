PATSCC := patscc
PATSCCFLAGS :=

##############################################################################

PROGS := hello tuple record factorial poly foreign datatype misc \
	exn array test_stack

tuple record factorial poly datatype: PATSCCFLAGS += -DATS_MEMALLOC_LIBC
exn array test_stack foreign:         PATSCCFLAGS += -DATS_MEMALLOC_LIBC

##############################################################################

all: $(PROGS)

check: hello
	@./$<

exn: loop.dats exn.dats
	$(PATSCC) $(PATSCCFLAGS) -o $@ $^

test_stack: stack.dats test_stack.dats
	$(PATSCC) $(PATSCCFLAGS) -o $@ $^

%: %.dats
	$(PATSCC) $(PATSCCFLAGS) -o $@ $< #-latslib

clean:
	rm -f *_?ats.c $(PROGS)

.PHONY: all check clean
