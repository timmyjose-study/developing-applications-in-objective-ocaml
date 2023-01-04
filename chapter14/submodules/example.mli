module type Hello_type = sig
  val message : string
  val hello : unit -> unit
end

module Hello : Hello_type

val goodbye : unit -> unit
val hello_goodbye : unit -> unit