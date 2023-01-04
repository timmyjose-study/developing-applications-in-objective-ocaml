module type GENSYM = sig
  val reset : unit -> unit
  val next : string -> string
end

module Gensym : GENSYM = struct
  let c = ref 0
  let reset () = c := 0

  let next s =
    incr c;
    s ^ string_of_int !c
end

module type USER_GENSYM = sig
  val next : string -> string
end

module UserGensym : USER_GENSYM = (Gensym : USER_GENSYM)
