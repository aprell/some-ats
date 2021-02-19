#include "share/atspre_define.hats"
#include "share/atspre_staload.hats"

datatype option (a : t@ype) =
  | Some of a
  | None // of ()

(* [case] warns if pattern matching is not exhaustive
   [case+] turns warnings into errors
   [case-] suppresses warnings *)
fn {a : t@ype} unwrap (n : option a) : a =
  case- n of Some v => v

// Equivalent definition:
fn {a : t@ype} unwrap' (n : option a) : a =
  let val- Some v = n in v end

fn test_option () =
  let
    val a = None // ()
    val b = Some {int} 42
  in
    (* Results in unmatched value at runtime:
       assertloc (unwrap_int a = 0) *)
    assertloc (unwrap b = 42)
  end

datatype list (a : t@ype) =
  | list_nil
  | list_cons of (a, list a)

fn {a : t@ype}
list_length (lst : list a) : int =
  let
    fun loop (lst : list a, acc : int) =
      case+ lst of
      (* Don't forget () in patterns:
         [int_list_nil] alone is treated as a variable pattern,
         which matches everything. *)
      | list_nil () => acc
      | list_cons (_, lst') => loop (lst', acc + 1)
  in
    loop (lst, 0)
  end

fun {a : t@ype} {b : t@ype}
list_map (f : a -> b, lst : list a) : list b =
  case+ lst of
  | list_nil () => list_nil
  | list_cons (hd, tl) => f hd \list_cons list_map (f, tl)

fun {a : t@ype} {b : t@ype}
list_fold_left (f : (a, b) -> a, init : a, lst : list b) : a =
  case+ lst of
  | list_nil () => init
  | list_cons (hd, tl) => list_fold_left (f, f (init, hd), tl)

fun {a : t@ype} {b : t@ype}
list_fold_right (f : (a, b) -> b, lst : list a, init : b) : b =
  case+ lst of
  | list_nil () => init
  | list_cons (hd, tl) => f (hd, list_fold_right (f, tl, init))

#define nil list_nil
#define ::  list_cons

fn {a : t@ype}
list_reverse (lst : list a) : list a =
  let fn flip_cons (xs : list a, x : a) = x :: xs in
    list_fold_left<list a><a> (flip_cons, nil, lst)
  end

fn {a : t@ype}
list_append (lst1 : list a, lst2 : list a) : list a =
  let fn cons (x : a, xs : list a) = x :: xs in
    list_fold_right<a><list a> (cons, lst1, lst2)
  end

fn test_list () =
  let
    val empty = nil
    val xs = 1 :: 2 :: 3 :: empty
    val ys = list_map<int>(*<int>*) (lam x => x + 3, xs)
    val zs = list_reverse (list_append (xs, ys))
    fn sum (lst : list int) : int =
      list_fold_right<int><int> (lam (a, b) => a + b, lst, 0)
  in
    assertloc (list_length empty = 0);
    assertloc (list_length xs = 3);
    assertloc (list_length ys = 3);
    assertloc (list_length zs = 6);
    assertloc (sum xs = 6);
    assertloc (sum ys = 15);
    assertloc (sum zs = 21)
  end

datatype expr =
  | Const of int
  | Binop of (binop, expr, expr) // [op] is a keyword

and binop = Add | Sub | Mul | Div

fun eval (e : expr) : int =
  case+ e of
  | Const n => n
  | Binop (o, e1, e2) => apply (o, eval e1, eval e2)

and apply (o : binop, a : int, b : int) : int =
  case+ o of
  | Add () => op+ (a, b) // a + b
  | Sub () => op- (a, b) // a - b
  | Mul () => op* (a, b) // a * b
  | Div () => op/ (a, b) // a / b

fn test_eval () =
  let
    val a = Const 1
    val b = Binop (Add, a, Const 1)
    val c = Binop (Sub, Const 5, b)
    val d = Binop (Div, Binop (Mul, c, Const 4), c)
  in
    assertloc (eval a = 1);
    assertloc (eval b = 2);
    assertloc (eval c = 3);
    assertloc (eval d = 4)
  end

// Generalized algebraic data types (GADTs)
// https://bluishcoder.co.nz/2018/10/16/generalized-algebraic-data-types-in-ats.html
datatype expr' (t@ype) =
  | Int (int) of int
  | Bool (bool) of bool
  | Add (int) of (expr' int, expr' int)
  | Eq (bool) of (expr' int, expr' int)

fun {a : t@ype} eval_ (e : expr' a) : a =
  case+ e of
  | Int n => n
  | Bool b => b
  | Add (e1, e2) => eval_ e1 + eval_ e2
  | Eq (e1, e2) => eval_ e1 = eval_ e2

fn test_eval' () =
  let
    val a = Int 1
    val b = Bool true
    val c = Add (a, Int 2)
    val d = Eq (c, Add (c, Int 1))
  in
    assertloc (eval_ a = 1);
    assertloc (eval_ b = true);
    assertloc (eval_ c = 3);
    assertloc (eval_ d = false)
  end

implement main0 () =
(
  test_option ();
  test_list ();
  test_eval ();
  test_eval' ()
)
