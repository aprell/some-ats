#include "share/atspre_define.hats"
#include "share/atspre_staload.hats"

// @ means unboxed (cannot be omitted?)
typedef point2D = @{x = double, y = double}

// ' means boxed, that is, heap allocated
typedef point3D = '{x = double, y = double, z = double}

// Pattern matching on record values
typedef person = @{name = string, age = int}
fn person_name (@{name = name, ...} : person) = name
fn person_age (@{age = age, ...} : person) = age

fn test_record () =
  let
    val a : point2D = @{x = 1.0, y = 2.0} // unboxed
    val b : point3D = '{x = 1.0, y = 2.0, z = 3.0} // boxed
    val p : person = @{name = "Bob", age = 25} // unboxed
  in
    assertloc (a.x = b.x);
    assertloc (a.y = b.y);
    assertloc (b.z = 3.0);
    assertloc (p.name = person_name p);
    assertloc (p.age = person_age p)
  end

implement main0 () =
  test_record ()
