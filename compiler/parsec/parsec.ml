open Core

module type Token = sig
  type t [@@deriving sexp_of, compare, equal]
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
  Stream with module Token = Token and type Chunk.t = Token.t list = struct
  type t = Token.t list ref [@@deriving sexp_of]

  module Token = Token

  module Chunk = struct
    type t = Token.t list [@@deriving sexp_of, compare, equal]
  end

  let of_chunk c = ref c

  let next t =
    match !t with
    | [] -> None
    | x :: xs ->
      t := xs;
      Some x
  ;;

  let peek t =
    match !t with
    | [] -> None
    | x :: _ -> Some x
  ;;

  let copy t = ref !t
end

module type Arg = sig
  module Data : sig
    type t [@@deriving sexp_of]
  end

  module Error : sig
    type t [@@deriving sexp_of]
  end

  module Stream : Stream
end

module Parse_result = struct
  type ('a, 'e) t =
    | Ok of 'a
    | Error of 'e
    | Fail
  [@@deriving sexp_of]

  let to_result_exn = function
    | Ok x -> Result.Ok x
    | Error e -> Error e
    | _ -> failwith "parsec: uncaught failure"
  ;;
end

module Make (Arg : Arg) = struct
  open Arg
  module Token = Stream.Token

  module Exceptions = struct
    exception Error of Error.t [@@deriving sexp_of]
    exception Fail [@@deriving sexp_of]
  end

  open Exceptions

  type env =
    { data : Data.t
    ; mutable stream : Stream.t
    }
  [@@deriving sexp_of]

  let with_env data stream f =
    let env = { data; stream } in
    match f env with
    | exception Error e -> Parse_result.Error e
    | exception Fail -> Parse_result.Fail
    | res -> Parse_result.Ok res
  ;;

  let stream env = env.stream
  let fail _ = raise_notrace Fail
  let error _ e = raise_notrace (Error e)

  type 'a t = env -> 'a

  let expect_eq t env =
    match Stream.next env.stream with
    | None -> fail env
    | Some t' when Token.equal t t' -> ()
    | Some _ -> fail env
  ;;

  let expect f env =
    match Stream.next env.stream with
    | Some t ->
      (match f t with
       | None -> fail env
       | Some x -> x)
    | None -> fail env
  ;;

  let orelse p1 p2 env =
    let s = Stream.copy env.stream in
    match p1 env with
    | res -> res
    | exception Fail ->
      env.stream <- s;
      p2 env
  ;;

  let pure x _env = x

  let cut e p env =
    match p env with
    | exception Fail -> error env e
    | x -> x
  ;;

  let map p ~f env = f (p env)

  let many p env =
    let rec loop acc =
      let s = Stream.copy env.stream in
      match p env with
      | exception Fail ->
        env.stream <- s;
        List.rev acc
      | x -> loop (x :: acc)
    in
    loop []
  ;;

  let some p env =
    let x = p env in
    x :: many p env
  ;;

  module Syntax = struct
    let ( <|> ) = orelse
    let ( <$> ) f p = map p ~f

    let ( <$ ) x p env =
      p env |> ignore;
      x
    ;;

    let ( $> ) p x env =
      p env |> ignore;
      x
    ;;

    let ( <* ) p1 p2 env =
      let x = p1 env in
      p2 env |> ignore;
      x
    ;;

    let ( *> ) p1 p2 env =
      p1 env |> ignore;
      p2 env
    ;;
  end

  let sep p ~by env =
    let open Syntax in
    let rec loop acc env =
      ((fun env ->
         by env;
         let x = p env in
         loop (x :: acc) env)
       <|> fun _env -> List.rev acc)
        env
    in
    ((fun env ->
       let x = p env in
       loop [ x ] env)
     <|> pure [])
      env
  ;;

  let optional p = Syntax.(Option.some <$> p <|> pure None)
  let either p1 p2 = Syntax.(Either.first <$> p1 <|> (Either.second <$> p2))
end
