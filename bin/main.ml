open Cmdliner

let run project_dir =
  let items = Lib.Parser.process_directory project_dir in
  let snippets = List.map Lib.Formatter.to_vscode_snippet items in
  let combined_json = `Assoc snippets in
  print_endline (Yojson.Basic.pretty_to_string combined_json)

let project_dir =
  let doc = "Path to the OCaml project directory" in
  Arg.(required & pos 0 (some dir) None & info [] ~docv:"PROJECT_DIR" ~doc)

let term = Term.(const run $ project_dir)

let cmd =
  let doc = "Extract VSCode snippets from OCaml project files" in
  let man =
    [
      `S Manpage.s_description;
      `P
        "This program extracts VSCode snippets from OCaml project files marked \
         with [@@snippet] attributes. The result will be printed to stdout in \
         JSON format.";
    ]
  in
  let info = Cmd.info "ocaml-snippet" ~version:"0.1.0" ~doc ~man in
  Cmd.v info term

let () = Cmd.eval cmd |> exit
