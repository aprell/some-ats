#include "share/atspre_define.hats"
#include "share/atspre_staload.hats"

// @ means unboxed (can be omitted, see below)
typedef pair_int_double = @(int, double)

// ' means boxed, that is, heap allocated
typedef pair_bool_string = '(bool, string)

fn test_tuple () =
  let
    val a : pair_int_double = (1, 2.0) // unboxed
    val b : pair_bool_string = '(true, "false") // boxed
  in
    assertloc (a.0 = 1);
    assertloc (a.1 = 2.0);
    assertloc (b.0 = true);
    assertloc (b.1 = "false");
  end

implement main0 () =
  test_tuple ()
