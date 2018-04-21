(*
 * Copyright yutopp 2017 - 2018.
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

type t = (kind * Loc.t)
and kind =
  | Lexing_error of Lexer.error_kind
  | Parsing_error
  | Not_found of string
  | Type_mismatch of {expect: Type.t; actual: Type.t}
  | Type_mismatch_struct of {actual: Type.t}
  | No_member of {ty: Type.t; name: string}
