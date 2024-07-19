open Ppxlib

(* Data structure to store the parsed result *)
type parsed_item =
  | Let_binding of string * string
  | Type_decl of string * string

(* Helper function to extract source code from a location and remove the attribute *)
let extract_source_code filename loc =
  let ic = open_in filename in
  let len = loc.loc_end.pos_cnum - loc.loc_start.pos_cnum in
  let buf = Bytes.create len in
  seek_in ic loc.loc_start.pos_cnum;
  really_input ic buf 0 len;
  close_in ic;
  let code = Bytes.to_string buf in
  (* Remove the [@@snippet payload] attribute *)
  let regex = Str.regexp "\\[@@snippet[^]]*\\]" in
  Str.global_replace regex "" code |> String.trim

(* Function to extract payload from an attribute *)
let extract_payload = function
  | PStr
      [
        {
          pstr_desc =
            Pstr_eval
              ({ pexp_desc = Pexp_ident { txt = Lident payload; _ }; _ }, _);
          _;
        };
      ] ->
      Some payload
  | _ -> None

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
          | Ppat_var { txt = _; _ } ->
              vb.pvb_attributes |> List.find_map (fun (attr : attribute) ->
                if String.equal attr.attr_name.txt "snippet" then
                  match extract_payload attr.attr_payload with
                  | Some payload -> Some (Let_binding (extract_source_code filename loc, payload))
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
              match extract_payload attr.attr_payload with
              | Some payload -> Some (Type_decl (extract_source_code filename loc, payload))
              | None -> None
            else
              None))
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
      | Let_binding (source_code, payload) ->
          Printf.printf "Found let binding in %s with payload '%s':\n%s\n"
            filename payload source_code
      | Type_decl (source_code, payload) ->
          Printf.printf "Found type declaration in %s with payload '%s':\n%s\n"
            filename payload source_code)
    items
