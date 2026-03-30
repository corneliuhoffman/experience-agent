open Lwt.Syntax

(* Unix domain socket server for receiving push updates from the MCP server.
   When Claude calls an MCP tool, the result is pushed here so the TUI
   can display it live. Fire-and-forget: MCP sends JSON, TUI acks. *)

type t = {
  socket_path : string;
  server_fd : Lwt_unix.file_descr;
  cancel : unit Lwt.u;
}

let handle_client ~on_message client_fd =
  Lwt.catch (fun () ->
    let ic = Lwt_io.of_fd ~mode:Lwt_io.Input client_fd in
    let oc = Lwt_io.of_fd ~mode:Lwt_io.Output client_fd in
    let* line = Lwt_io.read_line ic in
    let json = try Yojson.Safe.from_string line with _ -> `Null in
    let* () = on_message json in
    let* () = Lwt_io.write_line oc "{\"ok\":true}" in
    let* () = Lwt_io.flush oc in
    Lwt_unix.close client_fd
  ) (fun _exn ->
    Lwt.catch (fun () -> Lwt_unix.close client_fd) (fun _ -> Lwt.return_unit)
  )

let start ~on_message ~project_dir () =
  let sp = Urme_core.Paths.tui_socket_path ~project_dir in
  (try Unix.unlink sp with Unix.Unix_error _ -> ());
  let fd = Lwt_unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let addr = Unix.ADDR_UNIX sp in
  let* () = Lwt_unix.bind fd addr in
  Lwt_unix.listen fd 5;
  let cancel_promise, cancel = Lwt.wait () in
  Lwt.async (fun () ->
    let rec accept_loop () =
      let accept = Lwt_unix.accept fd |> Lwt.map (fun x -> `Accepted x) in
      let stopped = cancel_promise |> Lwt.map (fun () -> `Cancelled) in
      let* result = Lwt.pick [accept; stopped] in
      match result with
      | `Cancelled -> Lwt.return_unit
      | `Accepted (client_fd, _addr) ->
        Lwt.async (fun () -> handle_client ~on_message client_fd);
        accept_loop ()
    in
    accept_loop ()
  );
  Lwt.return { socket_path = sp; server_fd = fd; cancel }

let stop t =
  Lwt.wakeup_later t.cancel ();
  let* () = Lwt_unix.close t.server_fd in
  (try Unix.unlink t.socket_path with Unix.Unix_error _ -> ());
  Lwt.return_unit
