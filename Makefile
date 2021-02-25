PATSCC := patscc
PATSCCFLAGS += -D_GNU_SOURCE

# https://github.com/sparverius/ats-acc
ifneq ($(shell which acc),)
  ATSCC := acc pc
else
  ATSCC := $(PATSCC)
endif

##############################################################################

PROGS := \
  array \
  datatype \
  dependent \
  effects \
  exn \
  factorial \
  foreign \
  hello \
  misc \
  poly \
  proof \
  record \
  test_stack \
  tuple

$(filter-out hello effects misc proof,$(PROGS)): \
  PATSCCFLAGS += -DATS_MEMALLOC_LIBC

##############################################################################

all: $(PROGS)

check: hello
	@./$<

exn: loop.dats exn.dats
	$(ATSCC) $(PATSCCFLAGS) -o $@ $^

test_stack: stack.dats test_stack.dats
	$(ATSCC) $(PATSCCFLAGS) -o $@ $^

%: %.dats
	$(ATSCC) $(PATSCCFLAGS) -o $@ $< #-latslib

clean:
	rm -f *_?ats.c $(PROGS)

.PHONY: all check clean
