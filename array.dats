#include "share/atspre_define.hats"
#include "share/atspre_staload.hats"

// Arrays with size information

fn {a : t@ype} array (n : int, init : a) : arrszref a =
  let #define i2sz g0int2uint_int_size in
    arrszref_make_elt (i2sz n, init)
  end

fn {a : t@ype} array_iteri (f : (int, a) -<cloref1> void, arr : arrszref a) =
  let
    #define sz2i g0uint2int_size_int
    val len = sz2i (arr.size ())
    fun loop (i : int) =
      if i < len then
        (f (i, arr[i]); loop (i + 1))
  in
    loop 0
  end

fn {a : t@ype} array_iter (f : a -> void, arr : arrszref a) =
  array_iteri (lam (_, x) => f x, arr)

fn {a : t@ype} swap (arr : arrszref a, i : int, j : int) =
  let val tmp = arr[i] in arr[i] := arr[j]; arr[j] := tmp end

fn {a : t@ype} partition (arr : arrszref a, cmp : (a, a) -> int) =
  let
    val len = sz2i (arr.size ())
    val pivot = arr[0]
    fun loop (i : int, l : int) =
      if i < len then
        if cmp (arr[i], pivot) < 0 then
          (swap (arr, i, l + 1); loop (i + 1, l + 1))
        else loop (i + 1, l)
      else swap (arr, 0, l)
  in
    loop (1, 0)
  end

fn test () =
  let val a = array<int> (10, 0) in
    array_iteri (lam (i, _) =>
      a[i] := (case- i of // My goodness...
        | 0 => 3 | 1 => 1 | 2 => 4 | 3 => 1 | 4 => 5
        | 5 => 9 | 6 => 2 | 7 => 6 | 8 => 5 | 9 => 4), a);
    partition (a, lam (x, y) => compare (x, y));
    array_iter (lam v => print_int v, a)
  end

implement main0 () =
  test ()
