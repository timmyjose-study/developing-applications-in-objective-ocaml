class point (x_init, y_init) =
  object
    val mutable x = x_init
    val mutable y = y_init
    method get_x = x
    method get_y = y

    method moveto (a, b) =
      x <- a;
      y <- b

    method rmoveto (dx, dy) =
      x <- x + dx;
      y <- y + dy

    method distance () = sqrt (float ((x * x) + (y * y)))
    method to_string () = "(" ^ string_of_int x ^ ", " ^ string_of_int y ^ ")"
  end