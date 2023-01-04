open Log

module type ACCOUNT = sig
  type t

  exception Bad_operation

  val create : float -> float -> t
  val deposit : float -> t -> unit
  val withdraw : float -> t -> unit
  val balance : t -> float
end

module Account : ACCOUNT = struct
  type t = { mutable balance : float; overdraft : float }

  exception Bad_operation

  let create b o = { balance = b; overdraft = o }
  let deposit s c = c.balance <- c.balance +. s

  let withdraw s c =
    let ss = c.balance -. s in
    if ss < c.overdraft then raise Bad_operation else c.balance <- ss

  let balance c = c.balance
end

module type STATEMENT = sig
  type tdata
  type tinfo

  val edit_bank : tdata -> tinfo
  val edit_customer : tdata -> tinfo
end

module Fstatement (K : OKEY) (L : LOG with type tkey = K.t) = struct
  type tdata = L.t
  type tinfo = (L.tkey * L.tinfo) list

  let edit_bank h = List.map (fun n -> L.nth n h) [ 0; 1; 2; 3; 4 ]

  let edit_customer h =
    let c0 = K.of_string (string_of_float (Unix.time () -. 86400.)) in
    let f = K.lt c0 in
    L.get f h
end

module Fmanager
    (A : ACCOUNT)
    (K : OKEY)
    (L : LOG with type tkey = K.t and type tinfo = float)
    (S : STATEMENT
           with type tdata = L.t
            and type tinfo = (L.tkey * L.tinfo) list) =
struct
  type t = { accnt : A.t; log : L.t }

  let create s d = { accnt = A.create s d; log = L.create () }

  let deposit s g =
    A.deposit s g.accnt;
    L.add (K.create ()) s g.log

  let withdraw s g =
    A.withdraw s g.accnt;
    L.add (K.create ()) (-.s) g.log

  let balance g = A.balance g.accnt

  let statement edit g =
    let f (d, i) = K.to_string d ^ ":" ^ string_of_float i in
    List.map f (edit g.log)

  let statement_bank = statement S.edit_bank
  let statement_customer = statement S.edit_customer
end

module Manager =
  Fmanager (Account) (Date) (Flog (Date)) (Fstatement (Date) (Flog (Date)))

module type MANAGER_BANK = sig
  type t

  val create : float -> float -> t
  val deposit : float -> t -> unit
  val withdraw : float -> t -> unit
  val balance : t -> float
  val statement_bank : t -> string list
end

module Mbank : MANAGER_BANK with type t = Manager.t = Manager

module type MANAGER_CUSTOMER = sig
  type t

  val create : float -> float -> t
  val deposit : float -> t -> unit
  val withdraw : float -> t -> unit
  val balance : t -> float
  val statement_customer : t -> string list
end

module Mcustomer : MANAGER_CUSTOMER with type t = Manager.t = Manager