#include "share/atspre_define.hats"
#include "share/atspre_staload.hats"

fun factorial (n : int) : int =
  if n <= 1 then 1 else n * factorial (n - 1)

(* is syntactic sugar for:
   val rec factorial : int -> int =
     lam (n) => ... *)

(* Non-recursive functions can be defined with [fn]:
   fn square (n : int) : int = n * n desugars to
   val square : int -> int = lam (n) => n * n *)

fn factorial' (n : int) : int =
  let
    // Inner function loop is "envless"
    fun loop (i : int, acc : int) =
      if i <= 1 then acc else loop (i - 1, acc * i)
  in
    loop (n, 1)
  end

fn factorial'' (n : int) : int =
  let
    (* Inner function loop is not "envless"
       => must be compiled as closure
       [cloref1] optional because no heap allocation required? *)
    fun loop (i : int, acc : int) :<cloref1> int =
      if i <= n then loop (i + 1, acc * i) else acc
  in
    loop (1, 1)
  end

fn test (factorial : int -> int) =
((* begin *)
  assertloc (factorial 0 = 1);
  assertloc (factorial 1 = 1);
  assertloc (factorial 2 = 2);
  assertloc (factorial 3 = 6);
  assertloc (factorial 4 = 24);
  assertloc (factorial 5 = 120)
(* end *))

fn mk_counter () =
  let
    val count = ref<int> 1
    (* Function counter is not "envless"
       => must be compiled as heap-allocated closure
       [cloref1] required! *)
    fn counter () :<cloref1> int =
      let val old_count = !count in
        (!count := old_count + 1; old_count)
      end
  in
    counter
  end

fn count_to (n : int) =
  let
    val counter = mk_counter ()
    fun loop (i : int) =
      if i <= n then
        (println! (counter ()); loop (i + 1))
  in
    loop 1
  end

val () = count_to 10

// Defining a recursive function by back-patching
fn mk_factorial () =
  let
    val fact = ref<int -<cloref1> int> ($UNSAFE.cast 0)
    val () = !fact := (lam (n : int) : int =<cloref1>
      if n <= 1 then 1 else n * !fact (n - 1))
  in
    !fact
  end

// The closure returned by mk_factorial cannot be passed to test
fn test' (factorial : int -<cloref1> int) =
((* begin *)
  assertloc (factorial 0 = 1);
  assertloc (factorial 1 = 1);
  assertloc (factorial 2 = 2);
  assertloc (factorial 3 = 6);
  assertloc (factorial 4 = 24);
  assertloc (factorial 5 = 120)
(* end *))

implement main0 () =
((* begin *)
  test factorial;
  test factorial';
  test factorial'';
  test' (mk_factorial ())
(* end *))
