#include "share/atspre_define.hats"
#include "share/atspre_staload.hats"

staload "libats/ML/SATS/basis.sats"
staload "libats/ML/SATS/option0.sats"

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

datatype option_int = Some of int | None

extern typedef "option_int" = option_int

// [vtypedef]?
extern vtypedef "some" = Some_pstruct (int)
extern vtypedef "none" = None_pstruct ()

%{$
#include <assert.h>
#include <stdbool.h>

// typedef some_ *some;
// typedef none_ *none;

option_int safe_div(int a, int b)
{
    if (b != 0) {
        // Some of int => heap allocated
        some q = ATS_MALLOC(sizeof(some_));
        q->atslab__0 = a / b;
        return q;
    } else {
        // None (nullary) => NULL pointer
        return NULL;
    }
}

void test_div(void)
{
    assert(div_exn(4,  2) ==  2);
    assert(div_exn(4, -2) == -2);
    assert(div_exn(4,  0) ==  0);
}
%}

exception DivisionByZero

extern fn safe_div (a : int, b : int) : option_int = "ext#"
extern fn div_exn (a : int, b : int) : int = "ext#"
extern fn test_div () : void = "ext#"

(*
implement safe_div (a, b) =
  if b != 0 then Some (a / b) else None
*)

implement div_exn (a, b) =
  case+ safe_div (a, b) of
  | Some q => q
  | None () => raise DivisionByZero

datatype color = Red | Green | Blue

datatype expr =
  | Const of int
  | Add of (expr, expr)
  | Mul of (expr, expr)

extern typedef "color" = color
extern typedef "expr" = expr

%{$
#include <stdint.h>

int color_to_int(color c)
{
    return (int)(intptr_t)c;
}

// Assumes that the constructor tag comes first
struct header { int tag; };

int expr_to_int(expr e)
{
    return ((struct header *)e)->tag;
}
%}

extern fn color_to_int (c : color) : int = "ext#"
extern fn expr_to_int (e : expr) : int = "ext#"

fn test_tag () =
(
    assertloc (color_to_int Red = 0);
    assertloc (color_to_int Green = 1);
    assertloc (color_to_int Blue = 2);
    assertloc (expr_to_int (Const 1) = 0);
    assertloc (expr_to_int (Add (Const 1, Const 2)) = 1);
    assertloc (expr_to_int (Mul (Const 3, Const 4)) = 2);
)

implement main0 () =
(
  test_ext ();
  test_mac ();
  try test_div () with
  | ~DivisionByZero () => ();
  test_tag ();
)
