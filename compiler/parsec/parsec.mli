open Core

module type Token = sig
  type t [@@deriving sexp_of, equal, compare]

  val equal : t -> t -> bool
end

module type Chunk = sig
  type t [@@deriving sexp_of, compare, equal]
end

module type Stream = sig
  module Token : Token
  module Chunk : Chunk

  type t [@@deriving sexp_of]

  val of_chunk : Chunk.t -> t
  val next : t -> Token.t option
  val peek : t -> Token.t option
  val copy : t -> t
end

module Make_stream (Token : Token) :
  Stream with module Token = Token and type Chunk.t = Token.t list

module type Arg = sig
  module Data : sig
    type t [@@deriving sexp_of]
  end

  module Error : sig
    type t [@@deriving sexp_of]
  end

  module Stream : Stream
end

module Parse_result : sig
  type ('a, 'e) t =
    | Ok of 'a
    | Error of 'e
    | Fail
  [@@deriving sexp_of]

  val to_result_exn : ('a, 'e) t -> ('a, 'e) result
end

module Make (Arg : Arg) : sig
  open Arg
  module Token := Stream.Token

  type env [@@deriving sexp_of]
  type 'a t = env -> 'a

  module Exceptions : sig
    exception Error of Error.t [@@deriving sexp_of]
    exception Fail [@@deriving sexp_of]
  end

  val sep : 'a t -> by:unit t -> 'a list t
  val with_env : Data.t -> Stream.t -> (env -> 'a) -> ('a, Error.t) Parse_result.t
  val expect_eq : Token.t -> unit t
  val expect : (Token.t -> 'a option) -> 'a t
  val fail : env -> _
  val cut : Error.t -> 'a t -> 'a t
  val stream : env -> Stream.t
  val error : env -> Error.t -> _
  val orelse : 'a t -> 'a t -> 'a t
  val pure : 'a -> 'a t
  val optional : 'a t -> 'a option t
  val either : 'a t -> 'b t -> ('a, 'b) Either.t t
  val map : 'a t -> f:('a -> 'b) -> 'b t
  val many : 'a t -> 'a list t
  val some : 'a t -> 'a list t

  module Syntax : sig
    val ( <|> ) : 'a t -> 'a t -> 'a t
    val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t
    val ( <$ ) : 'a -> 'b t -> 'a t
    val ( $> ) : 'b t -> 'a -> 'a t
    val ( <* ) : 'a t -> 'b t -> 'a t
    val ( *> ) : 'a t -> 'b t -> 'b t
  end
end
