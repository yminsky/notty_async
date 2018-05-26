open! Base
open! Async

include struct
  open Notty
  module Unescape = Unescape
  module Tmachine = Tmachine
end

module Winch_listener = struct
  let waiting = ref []

  external winch_number : unit -> int = "caml_notty_winch_number" [@@noalloc]
  let sigwinch = Async.Signal.of_caml_int (winch_number ())

  let setup_winch = lazy (
    Signal.handle [sigwinch] ~f:(fun (_:Signal.t) ->
        List.iter !waiting ~f:(fun i -> Ivar.fill i ());
        waiting := []))

  let winch () =
    force setup_winch;
    let i = Ivar.create () in
    waiting := i :: !waiting;
    Ivar.read i
end

module Term = struct

  let bsize = 1024

  (* CR yminsky: Maybe turn this into just ac all that just reads the next
     input, rather than one that creates a pipe.  *)
  (* Call [f] function repeatedly as input is received from the
     stream. *)
  let process_input ~nosig ~stop ~on_input reader =
    let `Revert revert =
      let fd = Unix.Fd.file_descr_exn (Reader.fd reader) in
      Notty_unix.Private.setup_tcattr ~nosig fd
    in
    let flt  = Notty.Unescape.create () in
    let ibuf = Bytes.create bsize in
    let rec loop () =
      match Unescape.next flt with
      | #Unescape.event as input ->
        (* As long as there are events to read without blocking, dump
           them all into the pipe. *)
        if Deferred.is_determined stop then return ()
        else (on_input input; loop ())
      | `End   -> return ()
      | `Await ->
        match%bind Reader.read reader ibuf with
        | `Eof -> return ()
        | (`Ok n)  -> Unescape.input flt ibuf 0 n; loop ()
    in
    (* Some error handling to make sure that we call revert if the pipe fails *)
    let monitor = Monitor.create ~here:[%here] ~name:"Notty input pipe" () in
    don't_wait_for (Scheduler.within' ~monitor loop);
    don't_wait_for (Deferred.any
                      [ stop
                      ; Deferred.ignore (Monitor.get_next_error monitor)]
                    >>| revert)

  type t =
    { writer   : Writer.t
    ; tmachine : Tmachine.t
    ; buf      : Buffer.t
    ; fds      : Fd.t * Fd.t
    ; stop     : unit Deferred.t
    }

  let write t =
    Buffer.clear t.buf;
    Tmachine.output t.tmachine t.buf;
    Writer.write t.writer (Buffer.contents t.buf);
    Writer.flushed t.writer

  let refresh     t       = Tmachine.refresh  t.tmachine; write t
  let write_image t image = Tmachine.image    t.tmachine image; write t
  let set_cursor  t curs  = Tmachine.cursor   t.tmachine curs; write t

  let set_size    t dim   = Tmachine.set_size t.tmachine dim
  let size        t       = Tmachine.size     t.tmachine

  let release t =
    if Tmachine.release t.tmachine then write t else return ()

  let handle_resizes tmachine writer ~on_resize ~stop =
    don't_wait_for (
      match%bind Unix.isatty (Writer.fd writer) with
      | false -> return ()
      | true ->
        let rec loop () =
          match Fd.with_file_descr (Writer.fd writer) Notty_unix.winsize with
          | `Already_closed | `Error _ -> return ()
          | `Ok size ->
            match size with
            | None ->
              (* Note 100% clear that this is the right behavior,
                 since it's not clear why one would receive None from
                 winsize at all.  In any case, causing further resizes
                 should cause an app to recover if there's a temporary
                 inability to read the size. *)
              loop ()
            | Some size ->
              if Deferred.is_determined stop then return ()
              else (
                Tmachine.set_size tmachine size;
                Option.iter on_resize ~f:(fun f -> f size);
                let%bind () = Winch_listener.winch () in
                loop ())
        in
        loop ())

  let create
      ?(dispose=true)
      ?(nosig=true)
      ?(mouse=true)
      ?(bpaste=true)
      ?(reader=(force Reader.stdin))
      ?(writer=(force Writer.stdout))
      ?on_resize
      ~on_input
      ~stop
      ()
    =
    let (cap,size) =
      Fd.with_file_descr_exn (Writer.fd writer)
        (fun fd ->
           (Notty_unix.Private.cap_for_fd fd,
            Notty_unix.winsize fd))
    in
    let tmachine = Tmachine.create ~mouse ~bpaste cap in
    let buf = Buffer.create 4096 in
    let fds = (Reader.fd reader, Writer.fd writer) in
    process_input ~stop ~nosig ~on_input reader;
    handle_resizes tmachine writer ~on_resize ~stop;
    let t = { tmachine; writer; stop; buf; fds } in
    Option.iter size ~f:(set_size t);
    if dispose then Shutdown.at_shutdown (fun () -> release t);
    don't_wait_for (
      let%bind () = stop in
      release t);
    let%map () = write t in
    t

end

include Notty_unix.Private.Gen_output (struct
  type fd = Writer.t lazy_t and k = unit Deferred.t

  let def =
    Writer.stdout

  let to_fd w =
    (* CR yminsky: Here, we're defeating the purpose of Async's idioms
       for avoiding operating on closed FDs. To do something better,
       we'd need to adjust the API of Notty's Gen_output functor, or
       replicate it. *)
    match Fd.with_file_descr (Writer.fd (force w)) Fn.id with
    | `Already_closed | `Error _ -> raise_s [%message "Couldn't obtain FD"]
    | `Ok x -> x

  let write (lazy w) buf =
    let bytes = Buffer.contents_bytes buf in
    Writer.write_bytes w bytes ~pos:0 ~len:(Bytes.length bytes);
    Writer.flushed w
end)
