PATSCC := patscc
PATSCCFLAGS +=

##############################################################################

PROGS := \
  array \
  datatype \
  dependent \
  exn \
  factorial \
  foreign \
  hello \
  misc \
  poly \
  record \
  test_stack \
  tuple

$(filter-out dependent hello misc,$(PROGS)): \
  PATSCCFLAGS += -DATS_MEMALLOC_LIBC

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
