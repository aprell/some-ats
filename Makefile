PATSCC := patscc
PATSCCFLAGS :=

##############################################################################

PROGS := hello tuple record factorial poly foreign datatype misc exn

tuple record factorial poly datatype exn: PATSCCFLAGS += -DATS_MEMALLOC_LIBC

##############################################################################

all: $(PROGS)

check: hello
	@./$<

%: %.dats
	$(PATSCC) $(PATSCCFLAGS) -o $@ $< #-latslib

clean:
	rm -f *_?ats.c $(PROGS)

.PHONY: all check clean
