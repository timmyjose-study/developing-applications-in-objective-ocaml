let print_gc_stats () = Gc.print_stat stdout

let trigger_gc () =
  Gc.major ();
  Gc.minor ();
  Gc.full_major ();
  Gc.compact ()