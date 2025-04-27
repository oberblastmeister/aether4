open Std

module Child = struct
  type t =
    { pid : Pid.t
    ; stdin : Out_channel.t
    ; stdout : In_channel.t
    ; stderr : In_channel.t
    }

  let stdin t = t.stdin
  let stdout t = t.stdout
  let stderr t = t.stderr

  let wait t =
    Out_channel.close t.stdin;
    Core_unix.waitpid t.pid
  ;;

  let close t =
    Out_channel.close t.stdin;
    In_channel.close t.stdout;
    In_channel.close t.stderr
  ;;

  let wait_exn t =
    Out_channel.close t.stdin;
    protect ~f:(fun () -> Core_unix.waitpid_exn t.pid) ~finally:(fun () -> close t)
  ;;
end

let spawn ~prog ~args =
  let proc = Core_unix.create_process_env ~prog ~args ~env:(`Extend []) () in
  let child =
    Child.
      { pid = proc.pid
      ; stdin = Core_unix.out_channel_of_descr proc.stdin
      ; stdout = Core_unix.in_channel_of_descr proc.stdout
      ; stderr = Core_unix.in_channel_of_descr proc.stderr
      }
  in
  child
;;
