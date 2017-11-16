// Defines [list0] and [option0] among other things
staload "libats/ML/SATS/basis.sats"

// A boxed abstract data type
// There is also [abst@ype] for unboxed abstract data types
abstype stack (a : t@ype) // = ptr

exception StackEmpty

fn {a : t@ype} new () : stack a
fn {a : t@ype} empty (xs : stack a) : bool
fn {a : t@ype} push (xs : stack a, x : a) : stack a
fn {a : t@ype} peek (xs : stack a) : option0 a
fn {a : t@ype} peek_exn (xs : stack a) : a
fn {a : t@ype} pop (xs : stack a) : stack a
