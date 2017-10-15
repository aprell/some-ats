#include "share/atspre_define.hats"
#include "share/atspre_staload.hats"

datatype some_int =
  | None // of ()
  | Some of int

(* [case] warns if pattern matching is not exhaustive
   [case+] turns warnings into errors
   [case-] suppresses warnings *)
fn unwrap_int (n : some_int) : int =
  case- n of Some i => i

// Equivalent definition:
fn unwrap_int' (n : some_int) : int =
  let val- Some i = n in i end

fn test_some_int () =
  let
    val a = None // ()
    val b = Some 42
  in
    (* Results in unmatched value at runtime:
       assertloc (unwrap_int a = 0) *)
    assertloc (unwrap_int b = 42)
  end

datatype int_list =
  | int_list_nil
  | int_list_cons of (int, int_list)

fn int_list_length (lst : int_list) : int =
  let
    fun loop (lst : int_list, acc : int) =
      case+ lst of
      (* Don't forget () in patterns:
         [int_list_nil] alone is treated as a variable pattern,
         which matches everything. *)
      | int_list_nil () => acc
      | int_list_cons (_, lst') => loop (lst', acc + 1)
  in
    loop (lst, 0)
  end

fn test_int_list () =
  let
    val empty = int_list_nil
    val lst = int_list_cons (1, int_list_cons (2, int_list_cons (3, empty)))
  in
    assertloc (int_list_length empty = 0);
    assertloc (int_list_length lst = 3)
  end

datatype expr =
  | Lit of int
  | Binop of (binop, expr, expr) // [op] is a keyword

and binop = Add | Sub | Mul | Div

fun eval (e : expr) : int =
  case+ e of
  | Lit n => n
  | Binop (o, e1, e2) => apply (o, eval e1, eval e2)

and apply (o : binop, a : int, b : int) : int =
  case+ o of
  | Add () => op+ (a, b) // a + b
  | Sub () => op- (a, b) // a - b
  | Mul () => op* (a, b) // a * b
  | Div () => op/ (a, b) // a / b

fn test_eval () =
  let
    val a = Lit 1
    val b = Binop (Add, a, Lit 1)
    val c = Binop (Sub, Lit 5, b)
    val d = Binop (Div, Binop (Mul, c, Lit 4), c)
  in
    assertloc (eval a = 1);
    assertloc (eval b = 2);
    assertloc (eval c = 3);
    assertloc (eval d = 4)
  end

implement main0 () =
(
  test_some_int ();
  test_int_list ();
  test_eval ()
)
