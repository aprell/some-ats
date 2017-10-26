#include "share/atspre_define.hats"
#include "share/atspre_staload.hats"

// Arrays with size information

fn {a : t@ype} array (n : int, init : a) : arrszref a =
  arrszref_make_elt (g0int2uint_int_size n, init)

fn {a : t@ype} array_iteri (f : (size_t, a) -<cloref1> void, arr : arrszref a) =
  let
    #define i2sz g0int2uint_int_size
    val len = arr.size ()
    fun loop (i : size_t) =
      if i < len then
        (f (i, arr[i]); loop (i + i2sz 1))
  in
    // Unsigned literals?
    loop (i2sz 0)
  end

fn {a : t@ype} array_iter (f : a -> void, arr : arrszref a) =
  array_iteri (lam (_, x) => f x, arr)

fn {a : t@ype} swap (arr : arrszref a, i : size_t, j : size_t) =
  let val tmp = arr[i] in arr[i] := arr[j]; arr[j] := tmp end

fn {a : t@ype} partition (arr : arrszref a, cmp : (a, a) -> int) =
  let
    #define i2sz g0int2uint_int_size
    val len = arr.size ()
    val pivot = arr[0]
    fun loop (i : size_t, l : size_t) =
      if i < len then
        if cmp (arr[i], pivot) < 0 then
          (swap (arr, i, l + i2sz 1); loop (i + i2sz 1, l + i2sz 1))
        else
          loop (i + i2sz 1, l)
      else
        swap (arr, i2sz 0, l)
  in
    loop (i2sz 1, i2sz 0)
  end

fn test () =
  let
    #define sz2i g0uint2int_size_int
    val a = array<int> (10, 0)
  in
    array_iteri (lam (i, _) =>
      a[i] := (case- sz2i i of // My goodness...
        | 0 => 3 | 1 => 1 | 2 => 4 | 3 => 1 | 4 => 5
        | 5 => 9 | 6 => 2 | 7 => 6 | 8 => 5 | 9 => 4), a);
    partition (a, lam (x, y) => compare (x, y));
    array_iter (lam v => print_int v, a);
  end

implement main0 () =
(
  test ()
)
