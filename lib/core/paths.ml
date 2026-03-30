let tui_socket_path ~project_dir =
  let hash = Digest.to_hex (Digest.string project_dir) in
  Printf.sprintf "/tmp/urme-tui-%s.sock" (String.sub hash 0 8)
