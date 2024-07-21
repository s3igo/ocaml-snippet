[@@@ocamlformat "disable"]
let rec find_items filename =
  let module LocalTypes = Types in
  let open Ppxlib in
  function
  | [] -> []
  | item :: rest -> (
    match item with
    | { pstr_desc = Pstr_value (_, bindings); pstr_loc = loc; _ } ->
      let found_bindings =
        bindings |> List.filter_map (fun vb ->
          match vb.pvb_pat.ppat_desc with
          | Ppat_var { txt = _; _ } ->
              vb.pvb_attributes |> List.find_map (fun (attr : attribute) ->
                if String.equal attr.attr_name.txt "snippet" then
                  match Extractor.extract_payload attr.attr_payload with
                  | Some payload -> Some (LocalTypes.Let_binding (Extractor.extract_source_code filename loc, payload))
                  | None -> None
                else
                  None)
          | _ -> None)
      in
      found_bindings @ find_items filename rest
    | { pstr_desc = Pstr_type (_, type_decls); pstr_loc = loc; _ } ->
      let found_types =
        type_decls |> List.filter_map (fun td ->
          td.ptype_attributes |> List.find_map (fun (attr : attribute) ->
            if String.equal attr.attr_name.txt "snippet" then
              match Extractor.extract_payload attr.attr_payload with
              | Some payload -> Some (LocalTypes.Type_decl (Extractor.extract_source_code filename loc, payload))
              | None -> None
            else
              None))
      in
      found_types @ find_items filename rest
    | _ -> find_items filename rest)
[@@@ocamlformat "enable"]

let find_attributed_items filename =
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  let ast = Ppxlib.Parse.implementation lexbuf in
  close_in ic;
  find_items filename ast

let rec process_directory dir =
  let entries = Sys.readdir dir in
  entries
  |> Array.fold_left
       (fun acc entry ->
         let path = Filename.concat dir entry in
         if Sys.is_directory path then acc @ process_directory path
         else if Filename.check_suffix path ".ml" then
           acc @ find_attributed_items path
         else acc)
       []