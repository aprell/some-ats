#include "share/atspre_define.hats"
#include "share/atspre_staload.hats"

// Declarations
extern fun loop (body : () -<cloref1> void) : void
extern fn for_loop (i : int, n : int, body : int -<cloref1> void) : void

exception Break
exception Exit

// Difference between [raise] and [$raise]?
#define break (raise Break)
#define exit  (raise Exit)

// Definition
implement loop (body) =
  let val () = body () in loop body end

// Definition
implement for_loop (i, n, body) =
  let
    val i = ref<int> i
    val n = n
  in
    try loop (lam () =>
      if !i <= n then (body !i; !i := !i + 1) else exit)
    with
    // Exceptions are linear values
    | ~Break () => ()
    | ~Exit () => ()
  end
