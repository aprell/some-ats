#include "share/atspre_define.hats"
#include "share/atspre_staload.hats"

%{ // Inline C code
#define A 1
#define B 2

#define min(a, b) (((a) < (b)) ? (a) : (b))
#define max(a, b) (((a) > (b)) ? (a) : (b))

int sum_in_c(int a, int b)
{
    return a + b;
}
%}

// External names
extern fn sum (a : int, b : int) : int = "ext#sum_in_c"
extern fn min (a : int, b : int) : int = "mac#"
extern fn max (a : int, b : int) : int = "mac#"

// External values
macdef A = $extval (int, "A")
macdef B = $extval (int, "B")

fn test_ext () =
(
  assertloc (sum (A, B) = A + B);
  assertloc (sum (B, A) = B + A);
  assertloc (sum (~A, ~B) = ~(A + B));
  assertloc (sum (~B, ~A) = ~(B + A));
)

fn test_mac () =
(
  assertloc (min (1, 2) = 1);
  assertloc (min (2, 2) = 2);
  assertloc (min (3, 2) = 2);
  assertloc (max (1, 2) = 2);
  assertloc (max (2, 2) = 2);
  assertloc (max (3, 2) = 3);
)

fn test_extfcall () =
  // External function call
  $extfcall (void, "printf", "Hello %s!\n", "ATS");

implement main0 () =
(
  test_ext ();
  test_mac ();
)
