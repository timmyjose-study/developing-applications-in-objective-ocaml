class point (x_init, y_init) =
  object
    val mutable x = x_init
    val mutable y = y_init
    method get_x = x
    method get_y = y

    method moveto (new_x, new_y) =
      x <- new_x;
      y <- new_y

    method rmoveto (dx, dy) =
      x <- x + dx;
      y < y + dy

    method distance () = sqrt (float ((x * x) + (y * y)))
    method to_string () = "(" ^ string_of_int x ^ ", " ^ string_of_int y ^ ")"
  end

class picture n =
  object
    val mutable idx = -1
    val tab = Array.make n (new point (0, 0))

    method add p =
      try
        idx <- idx + 1;
        tab.(idx) <- p
      with Invalid_argument _ ->
        idx <- idx - 1;
        failwith ("picture:add:ind = " ^ string_of_int idx)

    method remove () = if idx > 0 then idx <- idx - 1

    method to_string () =
      if idx = -1 then "[]"
      else
        let s = ref "[" in
        for i = 0 to idx do
          if i > 0 then s := !s ^ ", ";
          s := !s ^ tab.(i)#to_string ()
        done;
        s := !s ^ "]";
        !s
  end