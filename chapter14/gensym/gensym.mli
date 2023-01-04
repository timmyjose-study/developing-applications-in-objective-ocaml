module type GENSYM = sig
  val reset : unit -> unit
  val next : string -> string
end

module Gensym : GENSYM

module type USER_GENSYM = sig
  val next : string -> string
end

module UserGensym: USER_GENSYM