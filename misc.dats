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

    symintr .plus
    overload .plus with plus
    val d = c.plus 5 // dot application
  in
    assertloc (c = 10);
    assertloc (d = 15)
  end

fn e () =
  assertloc (c = 10)
  where
  {
    fn plus (a : int, b : int) = a + b
    val a = plus (1, 2)
    val b = plus (3, 4)
    val c = a \plus b
  }

val () = a ()
val () = b ()
val () = c ()
val () = d ()
val () = e ()

implement main (argc, argv) =
  if argc > 1 then
    (print argv[1]; print_newline (); 0)
  else
    (print "No arguments\n"; 0)
