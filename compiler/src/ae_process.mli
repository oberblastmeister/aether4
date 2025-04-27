open Std

module Child : sig
  type t

  (*
     From the rust docs:
     The stdin handle to the child process, if any, will be closed before waiting. This helps avoid deadlock: it ensures that the child does not block waiting for input from the parent, while the parent waits for the child to exit.
  *)
  val wait : t -> Core_unix.Exit_or_signal.t
  val wait_exn : t -> unit
  val close : t -> unit
  val stdin : t -> Out_channel.t
  val stdout : t -> In_channel.t
  val stderr : t -> In_channel.t
end

val spawn : prog:string -> args:string list -> Child.t
