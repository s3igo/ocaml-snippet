(** VSCode Snippet Extractor for OCaml Projects

    This module provides functionality to extract VSCode snippets from OCaml project files.
    It searches for let bindings and type declarations marked with [@@snippet] attributes,
    and generates a JSON output compatible with VSCode's snippet format.

    Usage:
    ocaml-snippet <PROJECT_DIR>

    where <PROJECT_DIR> is the path to the OCaml project directory.
*)

open Ppxlib
open Cmdliner

(** Data structure to store the parsed result *)
type parsed_item =
  | Let_binding of string * string  (** Source code and payload for let bindings *)
  | Type_decl of string * string    (** Source code and payload for type declarations *)

(** Extract source code from a file given a location and remove the snippet attribute
    @param filename The name of the file to extract from
    @param loc The location of the code in the file
    @return The extracted and cleaned source code
*)
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

(** Extract payload from an attribute
    @param payload The attribute payload
    @return An option containing the extracted payload string, or None if extraction fails
*)
let extract_payload = function
  | PStr [ { pstr_desc = Pstr_eval (expr, _); _ } ] -> (
      match expr with
      | { pexp_desc = Pexp_constant (Pconst_string (s, _, _)); _ } -> Some s
      | { pexp_desc = Pexp_ident { txt = Lident s; _ }; _ } -> Some s
      | _ -> None)
  | _ -> None

(** Convert source code to VSCode snippet format
    @param payload The snippet identifier
    @param source_code The source code of the snippet
    @return A tuple containing the payload and the VSCode snippet JSON object
*)
let to_vscode_snippet payload source_code =
  let lines = String.split_on_char '\n' source_code in
  let body = `List (List.map (fun line -> `String line) lines) in
  ( payload,
    `Assoc
      [
        ("prefix", `String payload);
        ("body", body);
        ("description", `String "Generated snippet");
      ] )

[@@@ocamlformat "disable"]
(** Recursively traverse the AST and extract elements with specified attributes
    @param filename The name of the file being processed
    @param ast The abstract syntax tree to traverse
    @return A list of parsed items (let bindings and type declarations)
*)
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

(** Search for elements with specific attributes in source code
    @param filename The name of the file to process
    @return A list of parsed items from the file
*)
let find_attributed_items filename =
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  let ast = Parse.implementation lexbuf in
  close_in ic;
  find_items filename ast

(** Recursively process all .ml files in a directory
    @param dir The directory to process
    @return A list of parsed items from all .ml files in the directory and its subdirectories
*)
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

(** Main function to run the snippet extractor
    @param project_dir The root directory of the OCaml project
*)
let run project_dir =
  let items = process_directory project_dir in
  let snippets =
    items
    |> List.map (function
           | Let_binding (source_code, payload)
           | Type_decl (source_code, payload)
           -> to_vscode_snippet payload source_code)
  in
  let combined_json = `Assoc snippets in
  print_endline (Yojson.Basic.pretty_to_string combined_json)

(* Command line interface setup *)
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
         with [@@snippet] attributes. The result will be printed to stdout in JSON format.";
    ]
  in
  let info = Cmd.info "ocaml-snippet" ~version:"0.1.0" ~doc ~man in
  Cmd.v info term

(* Entry point *)
let () = Cmd.eval cmd |> exit
