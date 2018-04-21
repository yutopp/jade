(*
 * Copyright yutopp 2017 - 2018.
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open Batteries

module Env = Frontend_env

               (*
let type_check ~expect ~actual ctx loc =
  if expect <> actual then
    let e = Error.Type_mismatch {expect; actual} in
    Context.escape_with_error ctx e loc

let find ctx env tree =
  let id_s = Ast.string_of_id tree in
  match Env.find id_s env with
  | Some v -> v
  | None   -> Context.escape_with_error ctx (Error.Not_found id_s) tree.Ast.loc

let rec solve_type ctx env tree =
  match tree with
  | Ast.{kind = TypeSpec id} ->
     find ctx env id
  | _ ->
     failwith "TYP"

let rec pre_collect_types pre_env tree =
  match tree with
  | Ast.{kind = Module stmts} ->
     List.fold_left pre_collect_types pre_env stmts

  | _ ->
     pre_env

let guard_escape rec_fun tree =
  match rec_fun tree with
  | v -> ()
  | exception Context.Escape -> ()

let rec check tree =
  match tree with
  | T_ast.{kind = Module stmts; loc} ->
     stmts |> List.iter (guard_escape check)

  | T_ast.{kind = FuncDecl {params; body}; loc; ty} ->
     let (param_tys, ret_ty) = Type.as_func ty in
     check body

  | T_ast.{kind = ExprSeq exprs; loc} ->
     exprs |> List.iter check

  | T_ast.{kind = Let {expr}; loc} ->
     check expr

  | T_ast.{kind = Return e; loc} ->
     check e

  | T_ast.{kind = IfExpr {cond; then_c; else_c}; loc} ->
     check cond;
     check then_c;
     Option.may check else_c

  | Ast.{kind = ExprCall {func; args}; loc} ->
     ()

  | _ ->
     ()
                *)
