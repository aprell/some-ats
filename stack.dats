staload "libats/ML/SATS/basis.sats"
staload "libats/ML/SATS/list0.sats"
staload "libats/ML/DATS/list0.dats"

staload "stack.sats"

assume stack (a : t@ype) = list0 a

exception StackEmpty

implement {a} new () =
  list0_nil

implement {a} empty (xs) =
  case+ xs of
  | list0_nil () => true
  | list0_cons _ => false

implement {a} push (xs, x) =
  list0_cons (x, xs)

implement {a} peek (xs) =
  case+ xs of
  | list0_nil () => None0
  | list0_cons (x, _) => Some0 x

implement {a} peek_exn (xs) =
  case+ xs of
  | list0_nil () => raise StackEmpty
  | list0_cons (x, _) => x

implement {a} pop (xs) =
  case+ xs of
  | list0_nil () => list0_nil
  | list0_cons (_, xs) => xs

implement {a} print_stack (xs) =
(
  print "[";
  fprint (stdout_ref, xs, ", ");
  print "]\n";
)
