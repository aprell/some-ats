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

val () = a ()
val () = b ()
val () = c ()

implement main0 () = {}
