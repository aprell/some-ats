#include "share/atspre_define.hats"
#include "share/atspre_staload.hats"

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

implement main0 () =
  let
    val a = id 1
    val b = id 3.14
    val c = app ("ATS", id)
    val d = curry app 1 (lam (x : int) => x + 1)
    val e = uncurry (curry app) (2, (lam (x : int) => x * 2))
  in
    assertloc ((a : int) = 1);
    assertloc ((b : double) = 3.14);
    assertloc ((c : string) = "ATS");
    assertloc ((d : int) = 2);
    assertloc ((e : int) = 4)
  end
