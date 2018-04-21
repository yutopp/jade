(*
 * Copyright yutopp 2017 - 2018.
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open Batteries

module Type = struct
  type t =
    | Primitive of primitive
    | Compound of compound
    | Function of t list * t
    | Mut of t
    | Pointer of t
  and primitive =
    | Never
    | Unit
    | Int of {bits: int; signed: bool}
  and compound =
    | Struct of compound_struct
    | Tuple of t list
  and compound_struct = {
    name: string;
    mutable members: t list;
  }

  let show ty =
    match ty with
    | Primitive (Int {bits = 32; signed = true}) -> "i32"
    | Primitive (Int {bits = 1; signed = true}) -> "i1"
    | _ -> ""

  let pp f g fmt t =
    ()

  module Builtin = struct
    let never =
      Primitive Never

    let unit =
      Primitive Unit

    let int ~bits ~signed =
      Primitive (Int {bits; signed})

    let func ~params ~ret =
      Function (params, ret)
  end

  let ret_ty_of_func_ty ty =
    match ty with
    | Function (params, ret) ->
       ret
    | _ ->
       failwith "[ICE]"
end

module Type_env = struct
  type t = {
    env: (string, Type.t) Hashtbl.t;
    named: (string, unit) Hashtbl.t;
  }

  let empty () =
    {
      env = Hashtbl.create 0;
      named = Hashtbl.create 0;
    }

  let unify m ty_a ty_b =
    ()
end

module Ast = struct
  type t = {
    kind: kind;
    ty: Type.t;
    loc: Loc.t;
  }
  and kind =
    | ExprFunc of {param_specs: (string * Loc.t) list; body: t}
    | ExprFuncExternal of {param_specs: (string * Loc.t) list}
    | ExprSeq of t list
    | ExprLet of {name: string; expr: t}
    | ExprReturn of t option
    | ExprIf of {cond_e: t; then_e: t; else_e_opt: t option}
    | ExprStructIndex of {expr: t; index: int}
    | ExprCall of {func: t; args: t list}
    | ExprStruct of {members: t list}
    | LitNum of int
    | LitUnit
    | Var of string
  [@@deriving show]
end

module Func_ref = struct
  (* updated only when the building phase *)
  type t = {
    name: string;
    mutable state: state;
    mutable param_specs: param_spec list;
    mutable ret_ty: Type.t;
    mutable body: Ast.t option;
  }
  and state =
    | Complete
    | Prototype
  and param_spec = {
    param_name: string;
    param_ty: Type.t;
  }
  [@@deriving show]

  let create name =
    {
      name;
      state = Prototype;
      param_specs = [];
      ret_ty = Type.Builtin.unit;
      body = None;
    }
end

module Var_ref = struct
  type t = {
    mutable kind: kind;
  }
  and kind =
    | Undefined
    | Expr of Ast.t

  let create name =
    {
      kind = Undefined
    }
end

type t = {
  tenv: Type_env.t;
  venv: (string, Var_ref.t) Hashtbl.t;
}

let create () =
  {
    tenv = Type_env.empty ();
    venv = Hashtbl.create 0;
  }

let build_var_decl m name =
  let v = Var_ref.create name in
  Hashtbl.add m.venv name v

let build_named_struct_type m name =
  let ty = Type.Compound (Struct {name; members = []}) in
  Hashtbl.add m.tenv.env name ty

let lookup_type m name =
  let ty = Hashtbl.find m.tenv.env name in
  ty

let update_type m ty_a ty_b =
  Type_env.unify m.tenv ty_a ty_b

let iter_types f m =
  Hashtbl.iter f m.tenv.env

let create_param_spec ~loc name =
  (name, loc)

let create_expr_func ~ty ~loc ?(param_specs=[]) body =
  Ast.{kind = ExprFunc {param_specs; body}; ty; loc}

let create_expr_func_external ~ty ~loc ?(param_specs=[]) =
  Ast.{kind = ExprFuncExternal {param_specs}; ty; loc}

let update_var m name v =
  let v_ref = Hashtbl.find m.venv name in
  v_ref.Var_ref.kind <- Var_ref.Expr v

let create_expr_seq ~ty ~loc exprs =
  Ast.{kind = ExprSeq exprs; ty; loc}

let create_expr_let ~ty ~loc name expr =
  Ast.{kind = ExprLet {name; expr}; ty; loc}

let create_expr_return ~loc expr =
  let ty = Type.Builtin.never in
  Ast.{kind = ExprReturn expr; ty; loc}

let create_expr_if ~ty ~loc cond_e then_e else_e_opt =
  Ast.{kind = ExprIf {cond_e; then_e; else_e_opt}; ty; loc}

let build_indexed_access_struct ~ty ~loc expr index =
  Ast.{kind = ExprStructIndex {expr; index}; ty; loc}

let create_expr_call ~ty ~loc func args =
  Ast.{kind = ExprCall {func; args}; ty; loc}

let create_expr_struct ~ty ~loc members =
  Ast.{kind = ExprStruct {members}; ty; loc}

let create_lit_num ~ty ~loc v =
  Ast.{kind = LitNum v; ty; loc}

let create_lit_unit ~loc =
  let ty = Type.Builtin.unit in
  Ast.{kind = LitUnit; ty; loc}

let create_var ~ty ~loc s =
  Ast.{kind = Var s; ty; loc}
