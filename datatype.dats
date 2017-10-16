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

fn {a : t@ype} list_length (lst : list a) : int =
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

fn test_list () =
  let
    val empty = list_nil
    val lst = list_cons (1, list_cons (2, list_cons (3, empty)))
  in
    assertloc (list_length empty = 0);
    assertloc (list_length lst = 3)
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

implement main0 () =
(
  test_option ();
  test_list ();
  test_eval ()
)
