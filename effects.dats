#include "share/atspre_define.hats"
#include "share/atspre_staload.hats"

// A pure function
fn add1 (n : int) :<fun0> int = n + 1

// A function that may have side effects
fn add2 (n : int) :<fun1> int = n + 2

// fn add3 (n : int) :<fun0> int = add1 (add2 n)
//                                       ^~~~~~ Effects mismatch

fn add3 (n : int) :<fun1> int = add1 (add2 n)

fn add4 () : int -<fun1> int = lam n =<fun1> add2 (add2 n)

// A function that may not terminate
fun sum (n : int) :<!ntm> int =
  if n > 0 then n + sum (n - 1) else 0

// Proving termination
fun sum' {n : nat} .<n>. (n : int n) :<fun0> int =
  if n > 0 then n + sum' (n - 1) else 0

implement main0 () = ()
