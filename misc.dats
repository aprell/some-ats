#include "share/atspre_define.hats"
#include "share/atspre_staload.hats"

fn a () =
  let
    val () = println! "foo"
    val () = println! "foo"
    val () = println! "foo"
  in
    ()
  end

fn b () =
{
  val () = println! "bar"
  val () = println! "bar"
  val () = println! "bar"
}

fn c () =
(
  println! "baz";
  println! "baz";
  println! "baz"
)

fn d () =
  let
    fn plus (a : int, b : int) = a + b
    val a = plus (1, 2)
    val b = plus (3, 4)
    val c = a \plus b // infix application
  in
    assertloc (c = 10)
  end

val () = a ()
val () = b ()
val () = c ()
val () = d ()

implement main0 () = {}
