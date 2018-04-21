(*
 * Copyright yutopp 2017 - 2018.
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

type t = {
  kind: kind;
  ty: Mir.Type.t;
  loc: Loc.t
}
and kind =
  | Module of {nodes: t list}
  | Seq of {nodes: t list}
  | Let of {name: name; expr: t}
  | Call of {name: name; args: name list}
  | Assign of {lhs: name; rhs: name}
  | Return of name
  | ReturnVoid
  | IfStmt of {cond: name; then_c: t; else_c: t}
  | Num of int
  | Unit
  | Var of name
  | Init of name
  | DeclParam of name
  | ExprFunc of {params: name list; body: t} (* fix types of params *)
  | Undef
and name = string * int option
[@@deriving show]

let insert_let k_form k =
  match k_form with
  | {kind = Var id} ->
     k id
  | {loc} ->
     let new_id = Generator.fresh_id () in
     let name = (new_id, None) in
     let let_stmt = {kind = Let {name; expr = k_form}; ty = Mir.Type.Builtin.unit; loc} in
     match k name with
     | {kind = Seq {nodes}; ty; loc} ->
        {kind = Seq {nodes = let_stmt :: nodes}; ty; loc}
     | {ty; loc} as node ->
        {kind = Seq {nodes = [let_stmt; node]}; ty; loc}

let rec generate env ast =
  match ast with
  | Mir.Ast.{kind = ExprFunc {param_specs; body}; ty; loc} ->
     let env' =
       List.fold_left (fun e param_spec ->
                       let (param_name, _) = param_spec in
                       e (* TODO: add names to env *)
                      ) env param_specs
     in
     let params' =
       List.map (fun (n, loc) ->
                 (n, None)
                ) param_specs
     in
     let body' = generate env' body in
     {kind = ExprFunc {params = params'; body = body'}; ty; loc}

  | Mir.Ast.{kind = ExprLet {name; expr}; ty; loc} ->
     let name' = (name, None) in
     let expr' = generate env expr in
     {kind = Let {name = name'; expr = expr'}; ty; loc}

  | Mir.Ast.{kind = ExprSeq exprs; ty; loc} ->
     {kind = Seq {nodes = (List.map (generate env) exprs)}; ty; loc}

  | Mir.Ast.{kind = ExprReturn (Some e); ty; loc} ->
     let k = insert_let (generate env e) in
     k (fun e' ->
        {kind = Return e'; ty; loc})

  | Mir.Ast.{kind = ExprReturn None; ty; loc} ->
     {kind = ReturnVoid; ty; loc}

  | Mir.Ast.{kind = ExprIf {cond_e; then_e; else_e_opt}; ty; loc} ->
     let k = insert_let {kind = Undef; ty; loc} in (* result *)
     k (fun holder' ->
        let k = insert_let (generate env cond_e) in
        k (fun cond' ->
           let then_c_v =
             let k = insert_let (generate env then_e) in
             k (fun v' -> {kind = Assign {lhs = holder'; rhs = v'}; ty = Mir.Type.Builtin.unit; loc})
           in
           let else_c_v =
             match else_e_opt with
             | Some else_e ->
                let k = insert_let (generate env else_e) in
                k (fun v' -> {kind = Assign {lhs = holder'; rhs = v'}; ty = Mir.Type.Builtin.unit; loc})
             | None ->
                (* TODO: fix vars *)
                let rhs = ("unit<TMP>", None) in
                {kind = Assign {lhs = holder'; rhs}; ty = Mir.Type.Builtin.unit; loc}
           in
           let if_stmt = {
             kind = IfStmt {cond = cond'; then_c = then_c_v; else_c = else_c_v};
             ty = Mir.Type.Builtin.unit;
             loc
           } in
           let var = {kind = Var holder'; ty; loc} in
           {kind = Seq {nodes = [if_stmt; var]}; ty; loc}
          ))

  | Mir.Ast.{kind = ExprStructIndex {expr; index}; ty; loc} ->
     let k = insert_let (generate env expr) in
     k (fun expr' ->
        let (name', expr_idx_opt) = expr' in
        assert (None = expr_idx_opt);
        let place = (name', Some index) in
        {kind = Var place; ty; loc})

  | Mir.Ast.{kind = ExprCall {func = {kind = Var f}; args}; ty; loc} ->
     let rec bind xs args =
       match args with
       | [] ->
          let f' = (f, None) in
          {kind = Call {name = f'; args = List.rev xs}; ty; loc}
       | a :: args ->
          let k = insert_let (generate env a) in
          k (fun k -> bind (k :: xs) args)
     in
     bind [] args

  | Mir.Ast.{kind = ExprCall {func; args}; ty; loc} ->
     failwith "[ICE] not supported"

  | Mir.Ast.{kind = ExprStruct {members}; ty; loc} ->
     let k = insert_let {kind = Undef; ty; loc} in (* result *)
     k (fun holder' ->
        let rec bind subs_rev members =
          match members with
          | [] ->
             let assigns =
               List.mapi (fun index sub ->
                          let (holder_name, _) = holder' in
                          {kind = Assign {lhs = (holder_name, Some index); rhs = sub}; ty = Mir.Type.Builtin.unit; loc}
                         ) (List.rev subs_rev)
             in
             let var = {kind = Init holder'; ty; loc} in
             {kind = Seq {nodes = (var :: assigns) |> List.rev}; ty; loc}
          | member :: members' ->
             let k = insert_let (generate env member) in
             k (fun k -> bind (k :: subs_rev) members')
        in
        bind [] members
       )

  | Mir.Ast.{kind = LitNum n; ty; loc} ->
     {kind = Num n; ty; loc}

  | Mir.Ast.{kind = LitUnit; ty; loc} ->
     {kind = Unit; ty; loc}

  | Mir.Ast.{kind = Var s; ty; loc} ->
     let name = (s, None) in
     {kind = Var name; ty; loc}
