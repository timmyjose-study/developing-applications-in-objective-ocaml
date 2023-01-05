class virtual printable () =
  object (self)
    method virtual to_string : unit -> string
    method print () = print_string (self#to_string ())
  end

class rectangle (p1, p2) =
  object
    inherit printable ()
    val llc : point = p1
    val urc : point = p2
    method to_string () = "[" ^ llc#to_string () ^ ", " ^ urc#to_string () ^ "]"
  end

class point (x, y) =
  object
    inherit printable ()
    val mutable x = x
    val mutable y = y
    method get_x = x
    method get_y = y

    method moveto (new_x, new_y) =
      x <- new_x;
      y <- new_y

    method distance () = sqrt (float ((x * x) + (y * y)))

    method rmoveto (dx, dy) =
      x <- x + dx;
      y <- y + dy

    method to_string () = "(" ^ string_of_int x ^ ", " ^ string_of_int y ^ ")"
  end

class colored_point (x0, y0) c =
  object (self)
    inherit printable ()
    inherit point (x0, y0) as super
    method get_color = c
    method to_string () = super#to_string () ^ ": " ^ self#get_color
  end

class picture n =
  object
    inherit printable ()
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