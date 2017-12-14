#include "share/atspre_define.hats"
#include "share/atspre_staload.hats"

// Sorts, types of static terms
// A few subsorts of the sort [int]
sortdef nat = {a : int | a >= 0} // Natural numbers
sortdef pos = {a : int | a > 0}  // Positive numbers
sortdef neg = {a : int | a < 0}  // Negative numbers

// Existential quantification:
// "For some type n of sort nat, zero has the type int n"
val zero : [n : nat] int n = 0

typedef nat0 = [n : nat] int n

val one : nat0 = 1

// Universal quantification:
// "For every type a of sort int, given a value of type int a, return a value
// of type int (a + 1)"
fn incr {a : int} (n : int a) : int (a + 1) = n + 1

fn succ {a : nat} (n : int a) : int (a + 1) = incr n

fn add {a, b : nat} (n : int a, m : int b) : int (a + b) = n + m

fn abs {a : int} (n : int a) : nat0 =
  if n >= 0 then n else ~n

(* Mismatch of static terms
fn abs' (n : int) : nat0 =
  if n >= 0 then n else ~n
*)

sortdef sgn = {i : int | ~1 <= i && i <= 1}

// Note that [int] is overloaded:
// As a type of dynamic terms, [int] has sort [t@ype]
// As a type of static terms, [int] has sort [int -> t@ype]
fn sgn (n : int) : [a : sgn] int a =
  case+ n of
  | _ when n > 0 => 1
  | _ when n = 0 => 0
  | _ => ~1

fn silly (n : int 1) : int 42 = n + 41

// .<n>. is a termination metric
fun factorial {n : nat} .<n>. (n : int n) : int = // Why [int]?
  if n > 0 then n * factorial (n - 1) else 1

fn binsearch
  {n : nat}
  (arr : arrayref (int, n), n : int n, x : int) : int =
  let
    fun loop
      {i, j : int | 0 <= i && i <= j + 1 && j < n}
      (arr : arrayref (int, n), lo : int i, hi : int j) : int =
      if hi < lo then ~1
      else
        let
          val mid = lo + (hi - lo) / 2
          val m = arr[mid]
        in
          if x = m then mid
          else if x < m then loop (arr, lo, mid - 1)
          else (* x > m *) loop (arr, mid + 1, hi)
        end
  in
    loop (arr, 0, n - 1)
  end

// isqrt(n) = floor(sqrt(n))
fn isqrt
  {n : nat}
  (x : int n) : int =
  let
    fun loop
      {i, j : int | 0 <= i && i <= j + 1 && j <= n}
      (lo : int i, hi : int j) =
      if hi < lo then hi
      else
        let
          val mid = lo + (hi - lo) / 2
          // val x = x / mid fails type-checking; mid can be zero!
          val m = mid * mid
        in
          if x = m then mid
          else if x < m then loop (lo, mid - 1)
          else (* x > m *) loop (mid + 1, hi)
        end
  in
    loop (0, x)
  end

fn isqrt2
  {n : nat}
  (x : int n) : int =
  let
    fun loop
      {i, j : int | 1 <= i && i <= j + 1 && j <= n}
      (lo : int i, hi : int j) =
      if hi < lo then hi
      else
        let
          val mid = lo + (hi - lo) / 2
          val x = x / mid // mid can't be zero
        in
          if x = mid then mid
          else if x < mid then loop (lo, mid - 1)
          else (* x > mid *) loop (mid + 1, hi)
        end
  in
    if x = 0 then 0 else loop (1, x)
  end

fn test_isqrt () =
(
  assertloc (isqrt 0 = 0);
  assertloc (isqrt 1 = 1);
  assertloc (isqrt 2 = 1);
  assertloc (isqrt 3 = 1);
  assertloc (isqrt 4 = 2);
  assertloc (isqrt 5 = 2);
  assertloc (isqrt 6 = 2);
  assertloc (isqrt 7 = 2);
  assertloc (isqrt 8 = 2);
  assertloc (isqrt 9 = 3);
)

implement main0 () =
  test_isqrt ()
