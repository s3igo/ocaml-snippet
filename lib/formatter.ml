open Types

let to_vscode_snippet = function
  | Let_binding (source_code, payload) | Type_decl (source_code, payload) ->
      let lines = String.split_on_char '\n' source_code in
      let body = `List (List.map (fun line -> `String line) lines) in
      ( payload,
        `Assoc
          [
            ("prefix", `String payload);
            ("body", body);
            ("description", `String "Generated snippet");
          ] )
