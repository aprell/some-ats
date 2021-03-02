#include "share/atspre_define.hats"
#include "share/atspre_staload.hats"

// A view is a linear version of a prop

extern fn {a : t@ype}
ptr_get0
  {l : addr}
  (vw : a @ l | p : ptr l)
  : (a @ l | a)

extern fn {a : t@ype}
ptr_set0
  {l : addr}
  (vw : a? @ l | p : ptr l, x : a)
  : (a @ l | void)

fn {a : t@ype}
swap0
  {l1, l2 : addr}
  (vw1 : a @ l1, vw2 : a @ l2 | p1 : ptr l1, p2 : ptr l2)
  : (a @ l1, a @ l2 | void) =
  let
    // Threading views through calls
    val (vw1 | x1) = ptr_get0 (vw1 | p1)
    val (vw2 | x2) = ptr_get0 (vw2 | p2)
    val (vw1 | ()) = ptr_set0 (vw1 | p1, x2)
    val (vw2 | ()) = ptr_set0 (vw2 | p2, x1)
  in
    (vw1, vw2 | ())
  end

// !View1 >> View2 turns View1 into View2
// !View >> View can be shortened to !View

extern fn {a : t@ype}
ptr_get1
  {l : addr}
  (vw : !a @ l >> a @ l | p : ptr l)
  : a

extern fn {a : t@ype}
ptr_set1
  {l : addr}
  (vw : !a? @ l >> a @ l | p : ptr l, x : a)
  : void

fn {a : t@ype}
swap1
  {l1, l2 : addr}
  (vw1 : !a @ l1, vw2 : !a @ l2 | p1 : ptr l1, p2 : ptr l2)
  : void = {
    val x1 = ptr_get1 (vw1 | p1)
    val x2 = ptr_get1 (vw2 | p2)
    val () = ptr_set1 (vw1 | p1, x2)
    val () = ptr_set1 (vw2 | p2, x1)
  }

fn {a : t@ype}
swap1'
  {l1, l2 : addr}
  (vw1 : !a @ l1, vw2 : !a @ l2 | p1 : ptr l1, p2 : ptr l2)
  : void =
  // Let p : ptr L and pf be a proof of T@L
  // (1) val x = !p
  //     => val x = ptr_get1<T> (pf | p)
  // (2) !p := x
  //     => val () = ptr_set1<T> (pf | p, x)
  let val tmp = !p1 in
    !p1 := !p2; !p2 := tmp
  end

implement main0 () = ()
