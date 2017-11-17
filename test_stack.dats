#include "share/atspre_define.hats"
#include "share/atspre_staload.hats"

staload "libats/ML/SATS/basis.sats"
staload "libats/ML/SATS/list0.sats"
staload "libats/ML/DATS/list0.dats"
staload "libats/ML/SATS/option0.sats"
staload "libats/ML/DATS/option0.dats"

staload Stack = "stack.sats"
staload "stack.dats" // Required!
dynload "stack.dats"

fn test_stack () =
  let
    val s0 = $Stack.new ()
    val s1 = $Stack.push (s0, 1)
    val s2 = $Stack.push (s1, 2)
    val s3 = $Stack.push (s2, 3)
    val s4 = $Stack.pop s3
    val s5 = $Stack.pop s4
    val s6 = $Stack.pop s5
  in
    assertloc ($Stack.empty s0);
    assertloc (not ($Stack.empty s3));
    assertloc ($Stack.peek_exn<int> s3 = 3);
    assertloc ($Stack.peek_exn<int> s4 = 2);
    assertloc ($Stack.peek_exn<int> s5 = 1);
    assertloc (option0_is_some ($Stack.peek<int> s5));
    assertloc (option0_is_none ($Stack.peek<int> s6));
    assertloc ($Stack.empty s6);
    //$Stack.print s3;
  end

implement main0 () =
  test_stack ()
