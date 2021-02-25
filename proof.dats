#include "share/atspre_define.hats"
#include "share/atspre_staload.hats"

// Constructing a prop for static terms that represent proofs
// (int, int, int) -> prop
dataprop MUL (int, int, int) =
  | {n : int}
    MULbase (0, n, 0) // of ()
  | {m : nat} {n, p : int}
    MULind (m + 1, n, p + n) of MUL (m, n, p)
  | {m : pos} {n, p : int}
    MULneg (~m, n, ~p) of MUL (m, n, p)
//  ^~~~~~ A proof constructor

prfn mul_is_total
  {m, n : int} () : [p : int] MUL (m, n, p) =
  let
    prfun is_total
      {m : nat; n : int} .<m>. () : [p : int] MUL (m, n, p) =
      sif m > 0 then MULind (is_total {m - 1, n} ())
      else MULbase ()
  in
    sif m >= 0 then is_total {m, n} ()
//  ^~~~~~~~~ A conditional proof expression with a static test expression
    else MULneg (is_total {~m, n} ())
  end

// m * (n1 + n2) = m * n1 + m * n2
// prfn mul_is_distributive
//   {m, n1, n2, p1, p2 : int}
//   (pf1 : MUL (m, n1, p1), pf2 : MUL (m, n2, p2))
//   : MUL (m, n1 + n2, p1 + p2) =
//   // ...

// n * 0 = 0
extern praxi mul_nx0_0 {n : int} () : MUL(n, 0, 0)

// n * 1 = n
extern praxi mul_nx1_n {n : int} () : MUL(n, 1, n)

// m * -n = -(m * n)
extern praxi mul_neg_2 {m, n, p : int} (MUL (m, n, p)) : MUL(m, ~n, ~p)

// m * n = n * m
// prfn mul_is_commutative
//   {m, n, p : int}
//   (MUL (m, n, p))
//   : MUL (n, m, p) =

// Beautiful numbers
// https://bluishcoder.co.nz/2013/07/01/constructing-proofs-with-dataprop-in-ats.html
dataprop BEAUTIFUL (n : int) =
  | B_0 (0)
  | B_3 (3)
  | B_5 (5)
  | {m, n : nat} B_SUM (n + m) of (BEAUTIFUL m, BEAUTIFUL n)

// Proving that 0 and 3 are beautiful numbers
prfn zero_is_beautiful () : BEAUTIFUL 0 = B_0
prfn three_is_beautiful () : BEAUTIFUL 3 = B_3

// Stating that 5 is a beautiful number
extern praxi five_is_beautiful () : BEAUTIFUL 5

// Proving that 8 is a beautiful number
prfn eight_is_beautiful () : BEAUTIFUL 8 =
  B_SUM (three_is_beautiful (), five_is_beautiful ())

// Adding three to any beautiful number gives a beautiful number
prfn beautiful_plus_3 {n : nat} (b : BEAUTIFUL n) : BEAUTIFUL (n + 3) =
  B_SUM (b, three_is_beautiful ())

// Multiplying any beautiful number by two gives a beautiful number
prfn beautiful_times_2 {n : nat} (b : BEAUTIFUL n) : BEAUTIFUL (n * 2) =
  B_SUM (b, b)

// Multiplying any beautiful number n by a natural number m gives a beautiful number p
prfun beautiful_times_m {m, n : nat} .<m>. (b : BEAUTIFUL n)
  : [p : nat] (MUL (m, n, p), BEAUTIFUL p) =
  sif m == 0 then
    (MULbase, zero_is_beautiful ())
  else
    let prval (pf, b') = beautiful_times_m {m - 1, n} b in
      (MULind pf, B_SUM (b, b'))
    end

fn beautiful_add
  {a, b : nat}
  (pf1 : BEAUTIFUL a, pf2 : BEAUTIFUL b | a : int a, b : int b)
  : [c : nat] (BEAUTIFUL c | int c) =
  (B_SUM (pf1, pf2) | a + b)

prval pf1 = five_is_beautiful ()
prval pf2 = eight_is_beautiful ()
val (_ | n) = beautiful_add (pf1, pf2 | 5, 8)

implement main0 () = ()