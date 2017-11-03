PATSCC := patscc
PATSCCFLAGS :=

##############################################################################

PROGS := hello tuple record factorial poly foreign datatype misc \
	exn array

tuple record factorial poly datatype: PATSCCFLAGS += -DATS_MEMALLOC_LIBC
exn array:                            PATSCCFLAGS += -DATS_MEMALLOC_LIBC

##############################################################################

all: $(PROGS)

check: hello
	@./$<

exn: loop.dats exn.dats
	$(PATSCC) $(PATSCCFLAGS) -o $@ $^

%: %.dats
	$(PATSCC) $(PATSCCFLAGS) -o $@ $< #-latslib

clean:
	rm -f *_?ats.c $(PROGS)

.PHONY: all check clean
