let () =
  Printexc.record_backtrace true;
  let port = ref 8000 in
  let project_dir = ref "." in
  let web_port = ref 0 in
  let wipe = ref false in
  let import_session = ref "" in
  let init = ref false in
  let starting_from = ref "" in
  let prune = ref "" in
  let args = Array.to_list Sys.argv in
  let rec parse = function
    | "--chromadb-port" :: p :: rest ->
      port := int_of_string p;
      parse rest
    | "--project-dir" :: d :: rest ->
      project_dir := d;
      parse rest
    | "--web-port" :: p :: rest ->
      web_port := int_of_string p;
      parse rest
    | "--wipe" :: rest ->
      wipe := true;
      parse rest
    | "--import-session" :: id :: rest ->
      import_session := id;
      parse rest
    | "--init" :: rest ->
      init := true;
      parse rest
    | "--starting-from" :: date :: rest ->
      starting_from := date;
      parse rest
    | "--prune" :: date :: rest ->
      prune := date;
      parse rest
    | _ :: rest -> parse rest
    | [] -> ()
  in
  parse (List.tl args);
  let state = Experience_agent.Mcp_server.create_state
    ~port:!port ~project_dir:!project_dir in
  if !wipe then
    Lwt_main.run (Experience_agent.Mcp_server.wipe_db state)
  else if !import_session <> "" then
    Lwt_main.run (Experience_agent.Mcp_server.import_session state
      ~session_id:!import_session)
  else if !init then
    Lwt_main.run (Experience_agent.Mcp_server.init_from_history state
      ~starting_from:!starting_from)
  else if !prune <> "" then
    Lwt_main.run (Experience_agent.Mcp_server.prune_before state
      ~cutoff_str:!prune)
  else begin
    let tasks = [Experience_agent.Mcp_server.run_with_state state] @
      (if !web_port > 0 then
         [Experience_agent.Web_server.start ~port:!web_port ~state]
       else []) in
    Lwt_main.run (Lwt.pick tasks)
  end
