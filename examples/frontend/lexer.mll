(*
 * Copyright yutopp 2017 - 2018.
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

{
  open Lexing
  open Tokens

  type error_kind =
    | UnexpectedToken
    | IllegalEscapeChar
    | StringNotTerminated

  exception Error of error_kind

  module LitBuffer =
    struct
      type t = {
        start_p: position;
        buffer:  Buffer.t;
      }

      let create capacity lexbuf =
        {
          start_p = lexbuf.lex_start_p;
          buffer = Buffer.create capacity;
        }

      let add_char buf c =
        Buffer.add_char buf.buffer c

      let add_string buf str =
        Buffer.add_string buf.buffer str

      let settle_buffer buf lexbuf =
        lexbuf.lex_start_p <- buf.start_p;
        Buffer.contents buf.buffer
    end
}

let blank = [' ' '\t']+
let newline = "\r\n" | '\r' | '\n'

let numeric_10 = ['0'-'9']+
let numeric_16 = "0x" ['0'-'9' 'a'-'f' 'A'-'F']+
let num_bits = "8" | "16" | "32" | "64"

let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule token = parse
  | blank               { token lexbuf }
  | newline             { new_line lexbuf; token lexbuf }

  | "//"                { oneline_comment lexbuf }
  | "/*"                { multiline_comment lexbuf }

  | "module"            { KEYWORD_MODULE }
  | "struct"            { KEYWORD_STRUCT }
  | "extern"            { KEYWORD_EXTERN }
  | "def"               { KEYWORD_DEF }
  | "let"               { KEYWORD_LET }
  | "if"                { KEYWORD_IF }
  | "else"              { KEYWORD_ELSE }
  | "true"              { KEYWORD_TRUE }
  | "false"             { KEYWORD_FALSE }

  (* string state *)
  | '"'                 {
                          let buf = LitBuffer.create 80 lexbuf in
                          read_string_lit buf lexbuf
                        }

  | numeric_10 as i     { INT (int_of_string i, 32, true) }
  | numeric_16 as i     { INT (int_of_string i, 32, true) }

  | id as s             { ID s }

  | "==" as op          { BIN_OP op }
  | '+' as op           { BIN_OP (Char.escaped op) }
  | '-' as op           { BIN_OP (Char.escaped op) }
  | '*' as op           { BIN_OP (Char.escaped op) }
  | '/' as op           { BIN_OP (Char.escaped op) }

  | '='                 { EQ }

  | '('                 { LPAREN }
  | ')'                 { RPAREN }

  | '{'                 { LBLOCK }
  | '}'                 { RBLOCK }

  | '.'                 { DOT }
  | ','                 { COMMA }
  | ':'                 { COLON }
  | ';'                 { SEMICOLON }

  | eof                 { EOF }

  | _                   { raise (Error UnexpectedToken) }

and read_string_lit buf = parse
  | '"'             { let str = LitBuffer.settle_buffer buf lexbuf in STRING (str) }
  | '\\' '"'        { LitBuffer.add_char buf '"'; read_string_lit buf lexbuf }
  | '\\' '\\'       { LitBuffer.add_char buf '\\'; read_string_lit buf lexbuf }
  | '\\' 'n'        { LitBuffer.add_char buf '\n'; read_string_lit buf lexbuf }
  | '\\' 'r'        { LitBuffer.add_char buf '\r'; read_string_lit buf lexbuf }
  | '\\' 't'        { LitBuffer.add_char buf '\t'; read_string_lit buf lexbuf }
  | '\\' _          { raise (Error IllegalEscapeChar) }
  | [^ '"' '\\']+
    { LitBuffer.add_string buf (Lexing.lexeme lexbuf);
      read_string_lit buf lexbuf
    }
  | '\\' | eof      { raise (Error StringNotTerminated) }

and oneline_comment = parse
  | newline         { new_line lexbuf; token lexbuf }
  | eof             { EOF }
  | _               { oneline_comment lexbuf }

and multiline_comment = parse
  | "*/"            { token lexbuf }
  | newline         { new_line lexbuf; multiline_comment lexbuf }
  | eof             { EOF }
  | _               { multiline_comment lexbuf }
