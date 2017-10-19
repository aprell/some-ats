#include "share/atspre_define.hats"
#include "share/atspre_staload.hats"

implement main0 () =
  // External function call
  $extfcall (void, "printf", "Hello %s!\n", "ATS")
