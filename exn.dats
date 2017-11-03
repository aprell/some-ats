#include "share/atspre_define.hats"
#include "share/atspre_staload.hats"

staload "loop.dats"
dynload "loop.dats"

implement main0 () =
  for_loop (1, 10, lam (i : int) =>
    if i < 5 then print! i else break)
