let extract_source_code filename (loc : Ppxlib.location) =
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

let extract_payload = function
  | Ppxlib.PStr [ { pstr_desc = Pstr_eval (expr, _); _ } ] -> (
      match expr with
      | { pexp_desc = Pexp_constant (Pconst_string (s, _, _)); _ } -> Some s
      | { pexp_desc = Pexp_ident { txt = Lident s; _ }; _ } -> Some s
      | _ -> None)
  | _ -> None
