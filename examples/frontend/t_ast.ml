(*
 * Copyright yutopp 2017 - 2018.
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

type t = {
  kind: kind;
  ty: Type.t;
  loc: Loc.t
}
 and kind =
   | Module of t list
   | FuncDecl of {name: string; params: t list; body: t}
   | DeclStruct of {name: string; members: t list}
   | DeclExternFunc of {name: string; params: t list}
   | ExprSeq of t list
   | Let of {name: string; expr: t}
   | Return of t
   | IfExpr of {cond: t; then_c: t; else_c: t option}
   | ExprStructIndex of {expr: t; index: int}
   | ExprCall of {func: t; args: t list}
   | ExprCopy of t
   | ExprStruct of {members: t list}
   | Num of int
   | Unit
   | Bool of bool
   | Id of string
   | DeclParam of string
[@@deriving show]
