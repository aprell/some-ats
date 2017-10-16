#include "share/atspre_define.hats"
#include "share/atspre_staload.hats"

(* Function templates
   [t@ype] represents types of unspecified size *)

fn {a : t@ype} id (x : a) : a = x

fn {a, b : t@ype} app (arg : a, f : a -> b) : b = f arg

(* Currying returns closures
   fn {a, b : t@ype} app (arg : a) (f : a -> b) : b = f arg *)

fn {a, b, c : t@ype}
curry (f : (a, b) -> c) : a -<cloref1> b -<cloref1> c =
  lam (x : a) =<cloref1> lam (y : b) =<cloref1> f (x, y)

fn {a, b, c : t@ype}
uncurry (f : a -<cloref1> b -<cloref1> c) : (a, b) -<cloref1> c =
  lam (x : a, y : b) =<cloref1> f x y

fn test_templates () =
  let
    val a = id<int> 1
    val b = id<double> 3.14
    val c = app<string, string> ("ATS", id)
    val d = curry app 1 (lam (x : int) => x + 1)
    val e = uncurry (curry app) (2, (lam (x : int) => x * 2))
  in
    assertloc (a = 1);
    assertloc (b = 3.14);
    assertloc (c = "ATS");
    assertloc ((d : int) = 2);
    assertloc ((e : int) = 4)
  end

(* Polymorphic functions
   [type] represents boxed types (pointers) *)

fn id' {a : type} (x : a) : a = x

fn app' {a, b : type} (arg : a, f : a -> b) : b = f arg

typedef boxed_int = '(int)
typedef boxed_double = '(double)
typedef boxed_string = boxed string

fn test_poly () =
  let
    val a = id' {boxed_int} '(1)
    val b = id' {boxed_double} '(3.14)
    val f = lam (x : boxed_int) : boxed_int => '(x.0 * 2)
    val c = app' {boxed_int, boxed_int} ('(2), f)
    val d = app' {boxed_string, boxed_string} (box "ATS", id')
  in
    assertloc (a.0 = 1);
    assertloc (b.0 = 3.14);
    assertloc (c.0 = 4);
    assertloc (unbox d = "ATS")
  end

implement main0 () =
(
  test_templates ();
  test_poly ()
)
