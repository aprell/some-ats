#include "share/atspre_define.hats"
#include "share/atspre_staload.hats"

exception Break
exception Exit

// Difference between [raise] and [$raise]?
#define break (raise Break)
#define exit  (raise Exit)

fun loop (body : () -<cloref1> void) =
  let val () = body () in loop body end

fn for_loop (i : int, n : int, body : int -<cloref1> void) =
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

implement main0 () =
(
  for_loop (1, 10, lam (i : int) =>
    if i < 5 then print! i else break)
)
