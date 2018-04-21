(*
 * Copyright yutopp 2017 - 2018.
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open Batteries

let type_check ~expect ~actual ctx loc =
  if expect <> actual then
    let e = Error.Type_mismatch {expect; actual} in
    Context.escape_with_error ctx e loc

let expect_struct_type ~actual ctx loc =
  match actual with
  | Type.Struct _ ->
     ()
  | _ ->
     let e = Error.Type_mismatch_struct {actual} in
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

let guard_escape rec_fun ctx env tree =
  match rec_fun ctx env tree with
  | v -> v
  | exception Context.Escape -> env

let rec collect_toplevel_types env tree =
  match tree with
  | Ast.{kind = Module stmts} ->
     List.fold_left collect_toplevel_types env stmts

  | Ast.{kind = DefStruct {id}} ->
     let id_s = Ast.string_of_id id in
     let ty = Type.create_struct id_s in
     Env.add id_s ty env

  | _ ->
     env

let rec solve_incomplete_types ctx env tree =
  match tree with
  | Ast.{kind = Module stmts} ->
     List.fold_left (solve_incomplete_types ctx) env stmts

  | Ast.{kind = DefStruct {id; members}} ->
     let ty = find ctx env id in
     List.iteri (fun index member ->
                 let (name, ty_spec) = member in
                 let name_s = Ast.string_of_id name in
                 let member_ty = solve_type ctx env ty_spec in
                 Type.add_member_to_struct ty name_s index member_ty
                ) members;
     env

  | _ ->
     env

let rec collect_toplevel_functions ctx env tree =
  match tree with
  | Ast.{kind = Module stmts} ->
     List.fold_left (guard_escape collect_toplevel_functions ctx) env stmts

  | Ast.{kind = DefFunc {id; params; ret_spec}}
  | Ast.{kind = DeclExternFunc {id; params; ret_spec}} ->
     let param_tys =
       List.map (function
                  | Ast.{kind = DeclParam {ty_spec}; loc} ->
                     let ty = solve_type ctx env ty_spec in
                     ty
                  | _ ->
                     failwith "[ICE] not DeclParam"
                ) params
     in
     let ret_ty = solve_type ctx env ret_spec in
     let func_ty = Type.Function (param_tys, ret_ty) in

     let id_s = Ast.string_of_id id in
     Env.add id_s func_ty env

  | _ ->
     env

let rec analyze ctx env tree =
  match tree with
  | Ast.{kind = Module stmts; loc} ->
     let ty = Type.Builtin.i1 in
     let new_stmts =
       stmts
       |> List.filter_map
            (fun s ->
             match analyze ctx env s with
             | res                      -> Some (res |> fst)
             | exception Context.Escape -> None
            )
     in
     (T_ast.{kind = Module new_stmts; ty; loc}, env)

  | Ast.{kind = DefFunc {id; params; body}; loc} ->
     let ty = find ctx env id in
     let (_param_tys, ret_ty) = Type.as_func ty in

     let (new_params, env) = solve_function_params ctx env params in

     let (new_body, _) = analyze ctx env body in
     let () = type_check ~expect:ret_ty ~actual:new_body.T_ast.ty ctx body.Ast.loc in

     let ret_expr_ty = Type.Builtin.never in
     let ret_body = T_ast.{kind = Return new_body; ty = ret_expr_ty; loc} in

     let id_s = Ast.string_of_id id in
     (T_ast.{kind = FuncDecl {name = id_s; params = new_params; body = ret_body}; ty; loc}, env)

  | Ast.{kind = DefStruct {id; members}; loc} ->
     let ty = find ctx env id in

     let id_s = Ast.string_of_id id in
     let members' =
       List.map (fun member ->
                 let (name, ty_spec) = member in
                 let name_s = Ast.string_of_id name in
                 let ty = solve_type ctx env ty_spec in
                 T_ast.{kind = Id name_s; ty; loc} (* TODO: fix loc *)
                ) members
     in
     (T_ast.{kind = DeclStruct {name = id_s; members = members'}; ty; loc}, env)

  | Ast.{kind = DeclExternFunc {id; params}; loc} ->
     let ty = find ctx env id in

     let (new_params, env) = solve_function_params ctx env params in

     let id_s = Ast.string_of_id id in
     (T_ast.{kind = DeclExternFunc {name = id_s; params = new_params}; ty; loc}, env)

  | Ast.{kind = DeclParam {id; ty_spec}; loc} ->
     let ty = solve_type ctx env ty_spec in

     let id_s = Ast.string_of_id id in
     let env = Env.add id_s ty env in

     (T_ast.{kind = DeclParam id_s; ty; loc}, env)

  | Ast.{kind = ExprSeq exprs; loc} ->
     let (exprs_rev, env) =
       List.fold_left (fun (ns, e) expr ->
                       let (expr', e') = analyze ctx e expr in
                       (expr' :: ns, e')
                      ) ([], env) exprs
     in
     let ty = (List.hd exprs_rev).T_ast.ty in (* type of the last element *)
     (T_ast.{kind = ExprSeq (List.rev exprs_rev); ty; loc}, env)

  | Ast.{kind = ExprLet {id; expr}; loc} ->
     let (new_expr, env') = analyze ctx env expr in

     let id_s = Ast.string_of_id id in
     let env = Env.add id_s (new_expr.T_ast.ty) env in

     let ty = Type.Primitive (Unit) in
     (T_ast.{kind = Let {name = id_s; expr = new_expr}; ty; loc}, env)

  | Ast.{kind = ExprIf {cond; then_c; else_c}; loc} ->
     let (new_cond, env') = analyze ctx env cond in
     let new_then_c = then_c |> analyze ctx  env' |> fst in
     let new_else_c = else_c |> Option.map (analyze ctx  env' %> fst) in
     let ty = match new_else_c with
       | Some else_c_node ->
          (* TODO: unify *)
          new_then_c.T_ast.ty
       | None ->
          (* TODO: check else clause *)
          new_then_c.T_ast.ty
     in
     (T_ast.{kind = IfExpr {cond = new_cond; then_c = new_then_c; else_c = new_else_c}; ty; loc}, env)

  | Ast.{kind = ExprBlock e; loc} ->
     let new_e = analyze ctx env e |> fst in
     (* TODO: add scope *)
     (new_e, env)

  | Ast.{kind = ExprBinOp {op; lhs; rhs}; loc} ->
     let new_func = analyze ctx env op |> fst in
     let new_args = [lhs; rhs] |> List.map (analyze ctx env %> fst) in

     let func_ty = new_func.T_ast.ty in
     let (arg_tys, ret_ty) = Type.as_func func_ty in

     (T_ast.{kind = ExprCall {func = new_func; args = new_args}; ty = ret_ty; loc}, env)

  | Ast.{kind = ExprMember {expr; member}; loc} ->
     let expr' = analyze ctx env expr |> fst in
     let id_s = Ast.string_of_id member in

     let struct_ty = expr'.T_ast.ty in
     let () = expect_struct_type ~actual:struct_ty ctx loc in
     let members = Type.members_of_struct struct_ty in
     let spec =
       match Hashtbl.find_option members id_s with
       | Some spec ->
          spec
       | None ->
          let e = Error.No_member {ty = struct_ty; name = id_s} in
          Context.escape_with_error ctx e loc
     in
     (T_ast.{kind = ExprStructIndex {expr = expr'; index = spec.Type.index}; ty = spec.Type.ty; loc}, env)

  | Ast.{kind = ExprCall {func = Ast.{kind = Id "copy"}; args = [expr]}; loc} ->
     let expr' = analyze ctx env expr |> fst in
     (T_ast.{kind = ExprCopy expr'; ty = expr'.T_ast.ty; loc}, env)

  | Ast.{kind = ExprCall {func; args}; loc} ->
     let new_func = analyze ctx env func |> fst in
     let new_args = args |> List.map (analyze ctx env %> fst) in

     let func_ty = new_func.T_ast.ty in
     let (arg_tys, ret_ty) = Type.as_func func_ty in

     (T_ast.{kind = ExprCall {func = new_func; args = new_args}; ty = ret_ty; loc}, env)

  | Ast.{kind = ExprStruct {id; members}; loc} ->
     let struct_ty = find ctx env id in
     let () = expect_struct_type ~actual:struct_ty ctx loc in

     let m_initializer =
       List.fold_left (fun acc member ->
                       let (name, expr) = member in
                       let expr' = analyze ctx env expr |> fst in
                       let name_s = Ast.string_of_id name in
                       match Type.find_member_of_struct struct_ty name_s with
                       | Some member_spec ->
                          let Type.{index; ty = member_ty} = member_spec in
                          let () = type_check ~expect:member_ty ~actual:expr'.T_ast.ty ctx loc in
                          Map.add name_s (index, expr') acc
                       | None ->
                          let e = Error.No_member {ty = struct_ty; name = name_s} in
                          Context.escape_with_error ctx e loc
                      ) (Map.empty) members
     in
     let m_initializer =
       Hashtbl.fold (fun name member_spec acc ->
                     (*let Type.{index; ty} = member_spec in*)
                     match Map.mem name acc with
                     | true ->
                        acc
                     | false ->
                        failwith "TODO: add default initializer for the member"
                    ) (Type.members_of_struct struct_ty) m_initializer
     in
     let members' =
       m_initializer
       |> Map.values
       |> List.of_enum
       |> List.sort (fun (a_index, _) (b_index,_) -> compare a_index b_index)
       |> List.map snd
     in
     (T_ast.{kind = ExprStruct {members = members'}; ty = struct_ty; loc}, env)

  | Ast.{kind = LitInt {value; bits; signed}; loc} ->
     let ty = Type.Primitive (Int {bits; signed}) in
     (T_ast.{kind = Num value; ty; loc}, env)

  | Ast.{kind = LitUnit; loc} ->
     let ty = Type.Primitive (Unit) in
     (T_ast.{kind = Unit; ty; loc}, env)

  | Ast.{kind = Id id_s; loc} as id ->
     let ty = find ctx env id in
     (T_ast.{kind = Id id_s; ty; loc}, env)

  | _ ->
     failwith (Printf.sprintf "ANALYZER: %s" (Ast.show tree))

and solve_function_params ctx env params =
  let (new_params_rev, env') =
    List.fold_left (fun (px, e) p ->
                    let (p', e') = analyze ctx e p in
                    (p' :: px, e'))
                   ([], env) params
  in
  (new_params_rev |> List.rev, env')

let add_primitive_types env =
  let env =
    let ty_a = Type.fresh () in
    Env.add "=" (Type.Function ([ty_a; ty_a], ty_a)) env
  in
  let env = Env.add "+" (Type.Function ([Type.Builtin.i32; Type.Builtin.i32], Type.Builtin.i32)) env in
  let env = Env.add "-" (Type.Function ([Type.Builtin.i32; Type.Builtin.i32], Type.Builtin.i32)) env in
  let env = Env.add "*" (Type.Function ([Type.Builtin.i32; Type.Builtin.i32], Type.Builtin.i32)) env in
  let env = Env.add "==" (Type.Function ([Type.Builtin.i32; Type.Builtin.i32], Type.Builtin.i1)) env in
  let env = Env.add "unit" Type.Builtin.unit env in
  let env = Env.add "i32" Type.Builtin.i32 env in
  let env = Env.add "i1" Type.Builtin.i1 env in
  env

let rec generate ctx env tree =
  let env = add_primitive_types env in
  let env = collect_toplevel_types env tree in
  let env = solve_incomplete_types ctx env tree in
  let env = collect_toplevel_functions ctx env tree in
  analyze ctx env tree
