PATSCC := patscc
PATSCCFLAGS :=

##############################################################################

PROGS := hello tuple record factorial poly foreign

tuple record factorial poly: PATSCCFLAGS += -DATS_MEMALLOC_LIBC

##############################################################################

all: $(PROGS)

check: hello
	@./$<

%: %.dats
	$(PATSCC) $(PATSCCFLAGS) -o $@ $< #-latslib

clean:
	rm -f *_?ats.c $(PROGS)

.PHONY: all check clean
