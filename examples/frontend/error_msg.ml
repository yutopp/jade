(*
 * Copyright yutopp 2017 - 2018.
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

let string_of_kind kind =
  match kind with
  | Error.Lexing_error lex_kind ->
     let msg = match lex_kind with
       | _ -> ""
     in
     Printf.sprintf "Lexing error: %s" msg

  | Error.Parsing_error ->
     Printf.sprintf "Syntax error"

  | Error.Not_found s ->
     Printf.sprintf "Id not found in scope: %s" s

  | Error.Type_mismatch {expect; actual} ->
     Printf.sprintf "Type mismatch: expect=%s, actual=%s" (Type.show expect) (Type.show actual)

  | Error.Type_mismatch_struct {actual} ->
     Printf.sprintf "Type mismatch: struct expected, actual=%s" (Type.show actual)

  | Error.No_member {ty; name} ->
     Printf.sprintf "Type mismatch: struct %s has no member named %s" (Type.show ty) name

let to_msg err =
  let (kind, loc) = err in
  Printf.sprintf "%s: %s" (Loc.show_message loc) (string_of_kind kind)
