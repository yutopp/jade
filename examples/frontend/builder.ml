(*
 * Copyright yutopp 2017 - 2018.
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open Batteries

module Mir = Jade.Mir

let rec to_type_ref m ty =
  match ty with
  | Type.Primitive Never ->
     Mir.Type.Builtin.never
  | Type.Primitive Unit ->
     Mir.Type.Builtin.unit
  | Type.Primitive (Int {bits; signed}) ->
     Mir.Type.Builtin.int ~bits ~signed
  | Type.Function (params, ret) ->
     let params' = List.map (to_type_ref m) params in
     let ret' = to_type_ref m ret in
     Mir.Type.Builtin.func ~params:params' ~ret:ret'
  | Type.Struct (name, _specs) ->
     Mir.lookup_type m name
  | _ ->
     failwith "[ICE] unexpected type"

let to_param_spec node =
  match node with
  | T_ast.{kind = DeclParam name; ty; loc} ->
     let loc' = loc |> Loc.to_jade_loc in
     Mir.create_param_spec ~loc:loc' name
  | _ ->
     failwith "[ICE] unexpected node in param"

let rec build_top_level_pre m node =
  match node with
  | T_ast.{kind = Module nodes; loc} ->
     List.iter (build_top_level_pre m) nodes

  | T_ast.{kind = FuncDecl {name}; loc}
  | T_ast.{kind = DeclExternFunc {name}; loc} ->
     Mir.build_var_decl m name

  | T_ast.{kind = DeclStruct {name}; loc} ->
     Mir.build_named_struct_type m name

  | _ ->
     ()

let rec build_top_level m node =
  match node with
  | T_ast.{kind = Module nodes; ty; loc} ->
     List.iter (build_top_level m) nodes

  | T_ast.{kind = FuncDecl {name; params; body}; ty; loc} ->
     let param_specs = params |> List.map to_param_spec in

     let ty' = to_type_ref m ty in
     let body' = create_expr m body in
     let f = Mir.create_expr_func ~ty:ty' ~loc:(loc |> Loc.to_jade_loc) ~param_specs body' in
     Mir.update_var m name f

  | T_ast.{kind = DeclExternFunc {name; params}; ty; loc} ->
     let param_specs = params |> List.map to_param_spec in

     let ty' = to_type_ref m ty in
     let f = Mir.create_expr_func_external~ty:ty' ~loc:(loc |> Loc.to_jade_loc) ~param_specs in
     Mir.update_var m name f

  | T_ast.{kind = DeclStruct {name; members}; loc} ->
     let ty' = Mir.lookup_type m name in
     let spec = match ty' with
       | Mir.Type.Compound (Struct spec) -> spec
       | _ -> failwith "[ICE]"
     in

     let members' =
       List.map (fun member ->
                 let T_ast.{kind = id; ty; loc} = member in
                 let member_ty' = to_type_ref m ty in
                 member_ty'
                ) members
     in
     spec.Mir.Type.members <- members'

  | _ ->
     failwith "unknown top level"

and create_expr m node =
  match node with
  | T_ast.{kind = ExprSeq exprs; ty; loc} ->
     let ty' = to_type_ref m ty in
     let loc' = loc |> Loc.to_jade_loc in
     Mir.create_expr_seq ~ty:ty' ~loc:loc' (List.map (create_expr m) exprs)
  | T_ast.{kind = Let {name; expr}; ty; loc} ->
     let ty' = to_type_ref m ty in
     let loc' = loc |> Loc.to_jade_loc in
     Mir.create_expr_let ~ty:ty' ~loc:loc' name (create_expr m expr)
  | T_ast.{kind = Return expr; ty; loc} ->
     let loc' = loc |> Loc.to_jade_loc in
     Mir.create_expr_return ~loc:loc' (create_expr m expr |> Option.some)
  | T_ast.{kind = IfExpr {cond; then_c; else_c}; ty; loc} ->
     let ty' = to_type_ref m ty in
     let loc' = loc |> Loc.to_jade_loc in
     Mir.create_expr_if ~ty:ty'
                        ~loc:loc'
                        (create_expr m cond)
                        (create_expr m then_c)
                        (else_c |> Option.map (create_expr m))
  | T_ast.{kind = ExprStructIndex {expr; index}; ty; loc} ->
     let ty' = to_type_ref m ty in
     let loc' = loc |> Loc.to_jade_loc in
     let expr' = create_expr m expr in
     Mir.build_indexed_access_struct ~ty:ty'
                                     ~loc:loc'
                                     expr'
                                     index
  | T_ast.{kind = ExprCall {func; args}; ty; loc} ->
     let ty' = to_type_ref m ty in
     let loc' = loc |> Loc.to_jade_loc in
     Mir.create_expr_call ~ty:ty'
                          ~loc:loc'
                          (create_expr m func)
                          (args |> List.map (create_expr m))
  | T_ast.{kind = ExprStruct {members}; ty; loc} ->
     let ty' = to_type_ref m ty in
     let loc' = loc |> Loc.to_jade_loc in
     Mir.create_expr_struct ~ty:ty'
                            ~loc:loc'
                            (members |> List.map (create_expr m))
  | T_ast.{kind = Num v; ty; loc} ->
     let ty' = to_type_ref m ty in
     let loc' = loc |> Loc.to_jade_loc in
     Mir.create_lit_num ~ty:ty' ~loc:loc' v
  | T_ast.{kind = Bool b; ty; loc} ->
     let ty' = to_type_ref m ty in
     let loc' = loc |> Loc.to_jade_loc in
     Mir.create_lit_num ~ty:ty' ~loc:loc' (if b then 1 else 0)
  | T_ast.{kind = Unit; ty; loc} ->
     let loc' = loc |> Loc.to_jade_loc in
     Mir.create_lit_unit ~loc:loc'
  | T_ast.{kind = Id s; ty; loc} ->
     let ty' = to_type_ref m ty in
     let loc' = loc |> Loc.to_jade_loc in
     Mir.create_var ~ty:ty' ~loc:loc' s
  | _ ->
     failwith "[ICE] unexpected node in expr"

let build node =
  let m = Mir.create () in
  build_top_level_pre m node;
  build_top_level m node;
  m
