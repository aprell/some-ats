#include "share/atspre_define.hats"
#include "share/atspre_staload.hats"

// No support for variadic macros?
// Hack: Call external preprocessor
// #define printf(...) $extfcall (void, "printf", __VA_ARGS__)

implement main0 () =
  // External function call
  $extfcall (void, "printf", "Hello %s!\n", "ATS")
