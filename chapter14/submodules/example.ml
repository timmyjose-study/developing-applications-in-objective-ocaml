(* implicit module `Example` *)

module type Hello_type = sig
  val message : string
  val hello : unit -> unit
end

module Hello = struct
  let message = "Hello"
  let hello () = print_endline message
end

let goodbye () = print_endline "Goodbye"

let hello_goodbye () =
  Hello.hello ();
  goodbye ()