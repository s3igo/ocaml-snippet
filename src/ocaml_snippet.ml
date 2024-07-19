open Ppxlib

(* Data structure to store the parsed result *)
type parsed_item = Let_binding of string | Type_decl of string

(* Helper function to extract source code from a location and remove the attribute *)
let extract_source_code filename loc =
  let ic = open_in filename in
  let len = loc.loc_end.pos_cnum - loc.loc_start.pos_cnum in
  let buf = Bytes.create len in
  seek_in ic loc.loc_start.pos_cnum;
  really_input ic buf 0 len;
  close_in ic;
  let code = Bytes.to_string buf in
  (* Remove the [@@snippet] attribute *)
  let regex = Str.regexp "\\[@@snippet\\]" in
  Str.global_replace regex "" code |> String.trim

[@@@ocamlformat "disable"]
(* Function to traverse the AST and extract elements with specified attributes *)
let rec find_items filename = function
  | [] -> []
  | item :: rest -> (
    match item with
    | { pstr_desc = Pstr_value (_, bindings); pstr_loc = loc; _ } ->
      let found_bindings =
        bindings |> List.filter_map (fun vb ->
          match vb.pvb_pat.ppat_desc with
          | Ppat_var { txt = _; _ }
            when vb.pvb_attributes |> List.exists (fun (attr : attribute) ->
              String.equal attr.attr_name.txt "snippet")
                -> Some (Let_binding (extract_source_code filename loc))
          | _ -> None)
      in
      found_bindings @ find_items filename rest
    | { pstr_desc = Pstr_type (_, type_decls); pstr_loc = loc; _ } ->
      let found_types =
        type_decls |> List.filter_map (fun td ->
          match td.ptype_name.txt with
          | _
            when td.ptype_attributes |> List.exists (fun (attr : attribute) ->
              String.equal attr.attr_name.txt "snippet")
                -> Some (Type_decl (extract_source_code filename loc))
          | _ -> None)
      in
      found_types @ find_items filename rest
    | _ -> find_items filename rest)
[@@@ocamlformat "enable"]

(* Function to search for elements with specific attributes in source code *)
let find_attributed_items filename =
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  let ast = Parse.implementation lexbuf in
  close_in ic;
  find_items filename ast

(* Debug output *)
let () =
  let filename = "_example.ml" in
  let items = find_attributed_items filename in
  List.iter
    (function
      | Let_binding source_code ->
          Printf.printf "Found let binding in %s:\n%s\n" filename source_code
      | Type_decl source_code ->
          Printf.printf "Found type declaration in %s:\n%s\n" filename
            source_code)
    items
