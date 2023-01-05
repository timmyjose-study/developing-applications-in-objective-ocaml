class ['a, 'b] pair (x0 : 'a) (y0 : 'b) =
  object
    val mutable x = x0
    val mutable y = y0
    method fst = x
    method snd = y
  end

class ['a, 'b] acc_pair (x0 : 'a) (y0 : 'b) =
  object
    inherit ['a, 'b] pair x0 y0
    method get1 z = if x = z then y else raise Not_found
    method get2 z = if y = z then x else raise Not_found
  end

class point (x0, y0) =
  object
    val mutable x = x0
    val mutable y = y0
    method get_x = x
    method get_y = y

    method moveto (new_x, new_y) =
      x <- new_x;
      y <- new_y

    method rmoveto (dx, dy) =
      x <- x + dx;
      y <- y + dy

    method distance () = sqrt (float ((x * x) + (y * y)))
    method to_string () = "(" ^ string_of_int x ^ ", " ^ string_of_int y ^ ")"
  end

class point_pair (p1, p2) =
  object
    inherit [point, point] pair p1 p2
  end