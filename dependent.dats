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

(* Unsolved constraint
fun factorial (n : nat0) : nat0 =
  if n > 0 then n * factorial (n - 1) else 1
*)

implement main0 () = ()
