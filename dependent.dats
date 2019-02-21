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

// Why is the constraint {a : int} needed here?
fn abs {a : int} (n : int a) : nat0 =
  if n >= 0 then n else ~n

(* Mismatch of static terms
fn abs (n : int) : nat0 =
  if n >= 0 then n else ~n
*)

// Compiles just fine:
fn abs' {a : int} (n : int a) : nat0 =
  if n >= 0 then n + 2 else ~n + 1

// Better:
fn abs''
  {a : int; b : nat | a >= 0 && b == a || a < 0 && b == ~a}
  (n : int a) : int b =
  if n >= 0 then n else ~n

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
fun factorial {m : nat} .<m>. (n : int m) : int = // Why [int]?
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

fn swap
  {i, j, n : nat | 0 <= i && i < n; 0 <= j && j < n}
  (arr : arrayref (int, n), i : int i, j : int j) =
  let val tmp = arr[i] in
    arr[i] := arr[j];
    arr[j] := tmp
  end

(*
fn partition
  {n : pos}
  (arr : arrayref (int, n), n : int n) =
  let
    val pivot = arr[0]
    fun loop
      {i, l : nat | 1 <= i && i <= n; 0 <= l && l + 1 < n}
      (i : int i, l : int l) =
      if i < n then
        if arr[i] < pivot then
          (swap (arr, i, l + 1); loop (i + 1, l + 1))
        else loop (i + 1, l)
      else swap (arr, 0, l)
  in
    loop (1, 0)
  end
*)

// A dependent data type
// [t@ype+] indicates covariance (not sure if it matters)
datatype list (a : t@ype+, int) =
  // list_nil : {a : t@ype} () -> list (a, 0)
  | list_nil (a, 0) // of ()
  // list_cons : {a : t@ype} {n : nat} (a, list (a, n)) -> list (a, n + 1)
  | {n : nat} list_cons (a, n + 1) of (a, list (a, n))

fn {a : t@ype}
list_head
  {n : pos}
  (lst : list (a, n)) : a =
  case+ lst of // exhaustive
  | list_cons (hd, _) => hd

fn {a : t@ype}
list_tail
  {n : pos}
  (lst : list (a, n)) : list (a, n - 1) =
  case+ lst of // exhaustive
  | list_cons (_, tl) => tl

fn {a : t@ype}
list_length
  {n : nat}
  (lst : list (a, n)) : int n =
  let
    fun loop
      {i, j, n : nat | i + n == j}
      (lst : list (a, n), acc : int i) : int j =
      case+ lst of
      | list_nil () => acc
      | list_cons (_, lst') => loop (lst', acc + 1)
  in
    loop (lst, 0)
  end

fun {a : t@ype} {b : t@ype}
list_map
  {n : nat}
  (f : a -> b, lst : list (a, n)) : list (b, n) =
  case+ lst of
  | list_nil () => nil
  | list_cons (hd, tl) => f hd \list_cons list_map (f, tl)

fun {a : t@ype} {b : t@ype}
list_fold_left
  {n : nat}
  (f : (a, b) -> a, init : a, lst : list (b, n)) : a =
  case+ lst of
  | list_nil () => init
  | list_cons (hd, tl) => list_fold_left (f, f (init, hd), tl)

fun {a : t@ype} {b : t@ype}
list_fold_right
  {n : nat}
  (f : (a, b) -> b, lst : list (a, n), init : b) : b =
  case+ lst of
  | list_nil () => init
  | list_cons (hd, tl) => f (hd, list_fold_right (f, tl, init))

#define nil list_nil
#define ::  list_cons

fun {a : t@ype}
list_filter
  {n : nat}
  (lst : list (a, n), pred : a -> bool) : [m : nat | m <= n] list (a, m) =
  case+ lst of
  | list_nil () => nil
  | list_cons (hd, tl) =>
    if pred hd then hd :: list_filter (tl, pred)
    else list_filter (tl, pred)

fun {a : t@ype}
list_append
  {n, m : nat}
  (lst1 : list (a, n), lst2 : list (a, m)) : list (a, n + m) =
  case+ lst1 of
  | list_nil () => lst2
  | list_cons (hd, tl) => hd :: list_append (tl, lst2)

fun {a : t@ype}
list_reverse_append
  {n, m : nat}
  (lst1 : list (a, n), lst2 : list (a, m)) : list (a, n + m) =
  case+ lst1 of
  | list_nil () => lst2
  | list_cons (hd, tl) => list_reverse_append (tl, hd :: lst2)

fn {a : t@ype}
list_reverse
  {n : nat}
  (lst : list (a, n)) : list (a, n) =
  list_reverse_append (lst, nil)

fn test_list () =
  let
    val lst1 = 1 :: 2 :: 3 :: 4 :: 5 :: nil
    val lst2 = 6 :: 7 :: 8 :: 9 :: nil
    val lst3 = list_append (lst1, lst2)
    val lst4 = list_reverse lst3
    val lst5 = list_map<int> (lam x => x + 1, lst4)
  in
    assertloc (list_length lst1 = 5);
    assertloc (list_length lst2 = 4);
    assertloc (list_length lst3 = 9);
    assertloc (list_head<int> lst4 = 9);
    assertloc (list_head<int> (list_tail lst4) = 8);
    assertloc (list_fold_left<int><int> (lam (a, b) => a + b, 0, lst5) = 54);
  end

// The type of an ordered list of integers with values <= 100
// https://bluishcoder.co.nz/2010/09/01/dependent-types-in-ats.html
datatype ordered_list (int) =
  | ordered_list_nil (100) // of ()
  | {n, m : int | n <= m} ordered_list_cons (n) of (int n, ordered_list (m))

implement main0 () =
(
  test_isqrt ();
  test_list ()
)
