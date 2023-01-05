class point (x, y) =
  object
    val mutable x = x
    val mutable y = y
    method get_x = x
    method get_y = y

    method moveto (new_x, new_y) =
      x <- new_x;
      y <- new_y

    method rmoveto (dx, dy) =
      x <- x + dx;
      y <- y + dy

    method dist () = sqrt (float ((x * x) + (y * y)))
    method to_string () = "(" ^ string_of_int x ^ ", " ^ string_of_int y ^ ")"
  end

class colored_point (x, y) c =
  object (self)
    inherit point (x, y) as super
    val mutable c = c
    method get_color () = c
    method to_string () = super#to_string () ^ ": " ^ self#get_color ()
  end

class colored_point_1 coord c =
  object
    inherit colored_point coord c

    val true_colors =
      [ "white"; "blue"; "red"; "green"; "orange"; "chartreuse" ]

    method get_color () = if List.mem c true_colors then c else "UNKNOWN"
  end

class verbose_point p =
  object (self)
    inherit point p

    initializer
    let xm = string_of_int x and ym = string_of_int y in
    Printf.printf ">> Creation of a point at (%s, %s)\n" xm ym;
    Printf.printf " at a distance of %.3f from the origin\n" (self#dist ())
  end

class c1 =
  object
    initializer print_string "created an instance of class c1\n"
  end

class c2 =
  object
    inherit c1
    initializer print_string "created an instance of class c2\n"
  end

class c3 =
  object
    inherit c2
    initializer print_string "created an instance of class c3\n"
  end

class point_m1 (x0, y0) =
  object (self)
    inherit point (x0, y0) as super
    val mutable old_x = x0
    val mutable old_y = y0

    method private mem_pos () =
      old_x <- x;
      old_y <- y

    method undo () =
      x <- old_x;
      y <- old_y

    method moveto (new_x, new_y) =
      self#mem_pos ();
      super#moveto (new_x, new_y)

    method rmoveto (dx, dy) =
      self#mem_pos ();
      super#rmoveto (dx, dy)
  end


