(*
 * Copyright yutopp 2017 - 2018.
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

type t = {
  kind: kind;
  loc:  Loc.t;
}
 and kind =
   | Module of t list
   | SpecModule of t
   | DefFunc of {id: t; params: t list; ret_spec: t; body: t}
   | DefStruct of {id: t; members: (t * t) list}
   | DeclExternFunc of {id: t; params: t list; ret_spec: t}
   | DeclParam of {id: t; ty_spec: t}
   | TypeSpec of t
   | ExprSeq of t list
   | ExprLet of {id: t; expr: t}
   | ExprIf of {cond: t; then_c: t; else_c: t option}
   | ExprBlock of t
   | ExprBinOp of {op: t; lhs: t; rhs: t}
   | ExprStruct of {id: t; members: (t * t) list}
   | ExprCall of {func: t; args: t list}
   | ExprMember of {expr: t; member: t}
   | Id of string
   | LitString of string
   | LitInt of {signed: bool; bits: int; value: int}
   | LitUnit
[@@deriving show]

let string_of_id t =
  match t with
  | {kind = Id s} -> s
  | _             -> failwith "not an id node"

let string_of_lit_string t =
  match t with
  | {kind = LitString s} -> s
  | _                    -> failwith "not an literal(string) node"
