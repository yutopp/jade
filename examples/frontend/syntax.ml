(*
 * Copyright yutopp 2017 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open Batteries.Result

exception LexingError of Lexing.position * Lexer.error_kind
exception ParsingError of Lexing.position

let make_lexedbuf_from_input input =
  input |> BatIO.to_input_channel |> Lexing.from_channel

let make_ast ctx lexedbuf =
  try
    Some (Parser.program_entry Lexer.token lexedbuf)
  with
  | e ->
     let err = match e with
       | Lexer.Error kind -> Error.Lexing_error kind
       | Parser.Error -> Error.Parsing_error
       | _ -> failwith "[ICE] unexpected exception"
     in
     let start_p = Lexing.lexeme_start_p lexedbuf in
     let end_p = Lexing.lexeme_end_p lexedbuf in
     let loc = Loc.from_pos start_p end_p in
     Context.add_error ctx err loc;
     None

let make_ast_from_file ctx filepath =
  let make_ast_from_input input =
    input
    |> make_lexedbuf_from_input
    |> make_ast ctx
  in
  Batteries.File.with_file_in filepath make_ast_from_input
