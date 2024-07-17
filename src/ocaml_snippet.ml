open Ppxlib

(* Data structure to store the parsed result *)
type parsed_item =
  | Let_binding of string * attribute list
  | Function_decl of string * attribute list
  | Type_decl of string * attribute list
(* Other syntax elements defined similarly *)

[@@@ocamlformat "disable"]
(* Function to traverse the AST and extract elements with specified attributes *)
let rec find_items = function
  | [] -> []
  | item :: rest -> (
    match item with
    | { pstr_desc = Pstr_value (_, bindings); _ } ->
      let found_bindings =
        bindings |> List.filter_map (fun vb ->
          match vb.pvb_pat.ppat_desc with
          | Ppat_var { txt = name; _ }
            when vb.pvb_attributes |> List.exists (fun (attr : attribute) ->
              String.equal attr.attr_name.txt "my_attr")
                -> Some (Let_binding (name, vb.pvb_attributes))
          | _ -> None)
      in
      found_bindings @ find_items rest
    | _ -> find_items rest)
[@@@ocamlformat "enable"]

(* Function to search for elements with specific attributes in source code *)
let find_attributed_items filename =
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  let ast = Parse.implementation lexbuf in
  close_in ic;
  find_items ast

(* Debug output *)
let () =
  let items = find_attributed_items "_example.ml" in
  List.iter
    (function
      | Let_binding (name, _attrs) ->
          Printf.printf "Found let binding: %s\n" name
      | Type_decl (name, _attrs) ->
          Printf.printf "Found type declaration: %s\n" name
      | _ -> ())
    items
