(*
 * Copyright yutopp 2017 - 2018.
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open Batteries

module L = Llvm

type t = {
  llmodule: Llvm.llmodule;
}

type storage_t =
  | StoImm
  | StoStack
  | StoRet
  | StoIgnore
[@@deriving show]

type env_value_t = {ll: L.llvalue; sto: storage_t}

type env_t = (string, env_value_t) Map.t

type context_t = {
  llcontext: L.llcontext;
  llbuilder: L.llbuilder;
  lltypes: (string, Llvm.lltype) Hashtbl.t;
}

let lookup_lltype ctx name =
  let {lltypes} = ctx in
  Hashtbl.find lltypes name

let solve place env ctx =
  let (name, opt_index) = place in
  let v =
    try Map.find name env with
    | Not_found ->
       failwith (Printf.sprintf "[ICE] name not found: %s" name)
  in
  match opt_index with
  | Some index ->
     begin
       let {ll; sto} = v in
       match sto with
       | StoStack
       | StoRet ->
          let llelem = L.build_struct_gep ll index "" ctx.llbuilder in
          {ll = llelem; sto = StoStack}
       | StoImm ->
          failwith "[ICE] cannot take address of immidiate value"
       | StoIgnore ->
          failwith "[ICE] solve unexpected storage"
     end
  | None ->
     v

let initialize_backends =
  let is_initialized = ref false in
  let initialize () =
    match !is_initialized with
    | false ->
       Llvm_all_backends.initialize ();

       is_initialized := true
    | true ->
       ()
  in
  initialize

let is_stack_type ty =
  match ty with
  | Mir.Type.Compound (Struct _) ->
     true
  | _ ->
     false

let rec to_lltype ctx ty =
  let llctx = ctx.llcontext in
  match ty with
  | Mir.Type.Primitive (Mir.Type.Int {bits}) ->
     L.integer_type llctx bits
  | Mir.Type.Primitive Mir.Type.Unit ->
     L.void_type llctx
  | Mir.Type.Compound (Mir.Type.Struct spec) ->
     let Mir.Type.{
       name
     } = spec in
     lookup_lltype ctx name
  | Mir.Type.Function (param_tys, ret_ty) ->
     let param_ll_tys = param_tys |> List.map (to_lltype ctx) in
     let ret_ll_ty = ret_ty |> to_lltype ctx in

     let param_ll_tys' = match is_stack_type ret_ty with
       | false -> param_ll_tys
       | true  -> (L.pointer_type ret_ll_ty) :: param_ll_tys (* receiver type *)
     in
     let ret_ll_ty' = match is_stack_type ret_ty with
       | false -> ret_ll_ty
       | true  -> L.void_type llctx
     in
     L.function_type ret_ll_ty' (param_ll_tys' |> Array.of_list)
  | _ ->
     failwith ("[ICE] not supported type: " ^ (Mir.Type.show ty))

let make_context () =
  let () = initialize_backends () in

  let context = L.global_context () in
  let builder = L.builder context in
  {
    llcontext = context;
    llbuilder = builder;
    lltypes = Hashtbl.create 0;
  }

let declare_intrinsics ctx m llmod =
  let llctx = ctx.llcontext in
  let _ =
    (* http://llvm.org/docs/LangRef.html#llvm-memcpy-intrinsic *)
    let f = L.function_type (L.void_type llctx)
                            [|
                              L.pointer_type (L.i8_type llctx);
                              L.pointer_type (L.i8_type llctx);
                              (L.i32_type llctx);
                              (L.i32_type llctx);
                              (L.i1_type llctx);
                             |] in
    L.declare_function "llvm.memcpy.p0i8.p0i8.i32" f llmod
  in
  ()

module Intrinsics = struct
  let memcpy ctx llmod dest src len align is_volatile =
    let llctx = ctx.llcontext in
    let llbuilder = ctx.llbuilder in

    let dest_p =
      L.build_bitcast dest
                      (L.pointer_type (L.i8_type llctx))
                      ""
                      llbuilder
    in
    let src_p =
      L.build_bitcast src
                      (L.pointer_type (L.i8_type llctx))
                      ""
                      llbuilder
    in
    let lllen =
      L.const_int_of_string (L.i32_type llctx)
                            (string_of_int len) 10
    in
    let llalign =
      L.const_int_of_string (L.i32_type llctx)
                            (string_of_int align) 10
    in
    let f = L.lookup_function "llvm.memcpy.p0i8.p0i8.i32" llmod |> Option.get in
    L.build_call f
                 [|
                   dest_p;
                   src_p;
                   lllen;
                   llalign;
                   L.const_int (L.i1_type llctx) (Bool.to_int is_volatile);
                  |]
                 ""
                 llbuilder
end

let rec generate_type_proto ctx m name ty llmod parent_llty_opt =
  match ty with
  | Mir.Type.Compound (Struct spec) ->
     let Mir.Type.{
       name
     } = spec in
     let llty = L.named_struct_type ctx.llcontext name in
     Hashtbl.add ctx.lltypes name llty

  | _ ->
     ()

let rec generate_type ctx m name ty llmod parent_llty_opt =
  match ty with
  | Mir.Type.Compound (Struct spec) ->
     let Mir.Type.{
       name;
       members
     } = spec in
     let llty = L.type_by_name llmod name |> Option.get in
     let members' =
       List.map (fun ty -> to_lltype ctx ty) members
       |> Array.of_list
     in
     let is_a = false in (* TODO: fix *)
     L.struct_set_body llty members' is_a;

  | _ ->
     ()

let generate_static_proto ctx m name v llmod =
  match v.Mir.Var_ref.kind with
  | Mir.Var_ref.Undefined ->
     failwith "[ICE] still unknown"

  | Mir.Var_ref.Expr {kind = Mir.Ast.ExprFunc _; ty; loc} ->
     let fty = to_lltype ctx ty in
     let _llfv = L.declare_function name fty llmod in
     ()

  | Mir.Var_ref.Expr {kind = Mir.Ast.ExprFuncExternal _; ty; loc} ->
     let fty = to_lltype ctx ty in
     let _llfv = L.declare_function name fty llmod in
     ()

  | Mir.Var_ref.Expr e ->
     failwith "not supported yet (static proto)"

let rec generate_static ctx m name v llmod parent_llty_opt =
  match v.Mir.Var_ref.kind with
  | Mir.Var_ref.Undefined ->
     failwith "[ICE] still unknown"

  | Mir.Var_ref.Expr ({kind = Mir.Ast.ExprFunc _; ty; loc} as e) ->
     let env = Env.empty () in
     let k = K_normal.generate env e in

     let () = k |> K_normal.show |> Printf.printf "K: %s\n" in

     let passes = [
       Lir.complete_pass';
       Lir.replace_copy_move_pass';
       Lir.reduce_tmp_vars_pass';
       Lir.collect_stack_pass'
     ] in

     let ir_ctx = Lir.make_context () in
     let ir =
       let base = Lir.generate' ir_ctx env k in
       let () = base |> Lir.show_value |> Printf.printf "IR0: %s\n" in
       List.fold_left (fun e pass ->
                       let e' = pass ir_ctx env e in
                       let () = e' |> Lir.show_value |> Printf.printf "IRN: %s\n" in
                       e'
                      ) base passes
     in
     flush_all ();

     let env = Map.empty in
     let _ = generate_value ~recv_var:name ctx llmod env ir in
     ()

  | Mir.Var_ref.Expr ({kind = Mir.Ast.ExprFuncExternal _; ty; loc} as e) ->
     ()

  | Mir.Var_ref.Expr _ ->
     failwith "not supported yet (static)"

and generate ctx m =
  let name = "Rill" in (* TODO: change to ir_module name *)
  let llmod = L.create_module ctx.llcontext name in

  let () =
    declare_intrinsics ctx m llmod
  in

  let () =
    let build name ty =
      generate_type_proto ctx m name ty llmod None
    in
    Mir.iter_types build m
  in

  let () =
    let build name ty =
      generate_type ctx m name ty llmod None
    in
    Mir.iter_types build m
  in

  let () =
    let build name v =
      generate_static_proto ctx m name v llmod
    in
    Hashtbl.iter build m.Mir.venv
  in

  let () =
    let build name v =
      generate_static ctx m name v llmod None
    in
    Hashtbl.iter build m.Mir.venv
  in

  {
    llmodule = llmod;
  }

and generate_value ~recv_var ctx llmod env ir_value =
  let open Lir in
  match ir_value with
  | {kind = Function ({name; params; basic_blocks; _}, vars); ty} ->
     let ret_ty = Mir.Type.ret_ty_of_func_ty ty in
     let f = L.lookup_function recv_var llmod |> Option.get in
     let () =
       let param_names = List.map fst params in
       let param_names = match is_stack_type ret_ty with
         | false -> param_names
         | true  -> "__ret" :: param_names
       in
       List.iter2 L.set_value_name param_names (L.params f |> Array.to_list);
     in

     let env =
       List.fold_lefti (fun e index place ->
                        let (name, _) = place in
                        let llp = L.param f index in
                        let ev = {ll = llp; sto = StoImm} in
                        Map.add name ev e
                       ) env params
     in

     (* entry *)
     let entry_bb = L.append_block ctx.llcontext "entry" f in
     L.position_at_end entry_bb ctx.llbuilder;

     (* declare stack variables *)
     let env = match is_stack_type ret_ty with
       | false -> env
       | true  ->
          let v = {ll = (L.param f 0); sto = StoRet} in
          Map.add "__ret" v env
     in
     let env =
       VarsMap.fold
         (fun k v acc ->
          let storage = match v with
            | (Mir.Type.Primitive Unit, _) -> StoIgnore (* unit will not be exported to executable *)
            | (ty, Lir.MutVar) ->
               if k = "__ret" && (is_stack_type ty) then StoRet else StoStack
            | (ty, Lir.MutImm) when is_stack_type ty ->
               if k = "__ret" && (is_stack_type ty) then StoRet else StoStack
            | (_ , Lir.MutImm) -> StoImm
          in
          Printf.printf "VAR %s, storage=%s\n" k (show_storage_t storage);
          let (ty, _) = v in
          let llty = to_lltype ctx ty in

          match storage with
          | StoRet ->
             acc
          | StoStack ->
             let llv = L.build_alloca llty k ctx.llbuilder in
             let ev = {ll = llv; sto = storage} in
             Map.add k ev acc
          | _ ->
             let ev = {ll = L.undef llty; sto = storage} in
             Map.add k ev acc
         ) vars env
     in

     (* basic blocks *)
     let bb_env =
       Vect.fold_left
         (fun e bb ->
          let {index} = bb in
          let name = "" in
          let llbb = L.append_block ctx.llcontext name f in
          Map.add index llbb e
         ) Map.empty basic_blocks
     in

     (* connect entry and the first basic block *)
     L.position_at_end entry_bb ctx.llbuilder;
     let _ = L.build_br (Map.find 0 bb_env) ctx.llbuilder in

     (* actual instructions *)
     let _ =
       Vect.fold_left
         (fun e bb ->
          let {index; insts; terminator} = bb in

          let llbb = Map.find index bb_env in
          L.position_at_end llbb ctx.llbuilder;

          let next_e =
            Vect.fold_left
              (fun e i ->
               match generate_inst ctx llmod e i with
               | Some (_, ne) -> ne
               | None -> e
              ) e insts
          in
          Option.may (fun t ->
                      generater_terminator ctx llmod next_e bb_env t |> ignore
                     ) terminator;

          next_e
         ) env basic_blocks
     in
     L.dump_value f;
     flush_all ();
     (f, StoStack)

  | {kind = Const (ValueInt v); ty} ->
     (L.const_int (to_lltype ctx ty) v, StoImm)

  | {kind = Const ValueUndef; ty} ->
     let llty = to_lltype ctx ty in
     (L.undef llty, StoImm)

  (* TODO: FIX *)
  | {kind = Call (("=", _), [lhs; rhs])} ->
     let {ll = lhs_v; sto} = solve lhs env ctx in
     let {ll = rhs_v; sto} = solve rhs env ctx in
     (L.build_add lhs_v rhs_v "" ctx.llbuilder, StoImm)

  (* TODO: FIX *)
  | {kind = Call (("+", _), [lhs; rhs])} ->
     let {ll = lhs_v; sto} = solve lhs env ctx in
     let {ll = rhs_v; sto} = solve rhs env ctx in
     (L.build_add lhs_v rhs_v "" ctx.llbuilder, StoImm)

  (* TODO: FIX *)
  | {kind = Call (("-", _), [lhs; rhs])} ->
     let {ll = lhs_v; sto} = solve lhs env ctx in
     let {ll = rhs_v; sto} = solve rhs env ctx in
     (L.build_sub lhs_v rhs_v "" ctx.llbuilder, StoImm)

  (* TODO: FIX *)
  | {kind = Call (("==", _), [lhs; rhs])} ->
     let {ll = lhs_v; sto} = solve lhs env ctx in
     let {ll = rhs_v; sto} = solve rhs env ctx in
     (L.build_icmp L.Icmp.Eq lhs_v rhs_v "" ctx.llbuilder, StoImm)

  (* TODO: FIX *)
  | {kind = Call (("*", _), [lhs; rhs])} ->
     let {ll = lhs_v; sto} = solve lhs env ctx in
     let {ll = rhs_v; sto} = solve rhs env ctx in
     (L.build_mul lhs_v rhs_v "" ctx.llbuilder, StoImm)

  (* TODO: FIX *)
  | {kind = Call ((func_name, _), arg_names); ty} ->
     Printf.printf "FIND=%s\n" func_name;
     let f = L.lookup_function func_name llmod |> Option.get in

     let arg_names =
       match is_stack_type ty with
       | true ->
          (recv_var, None) :: arg_names (* add value receiver *)
       | false ->
          arg_names
     in

     let args =
       arg_names
       |> List.map (fun n -> solve n env ctx)
       |> List.map (fun {ll; sto} -> ll)
       |> Array.of_list
     in

     (L.build_call f args "" ctx.llbuilder, StoImm)

  | {kind = Move place}
  | {kind = Copy place}
  | {kind = Var place} ->
     let {ll; sto} = solve place env ctx in
     (ll, sto)

  | {kind = Unit} ->
     (L.undef (L.void_type ctx.llcontext), StoIgnore)

and generate_inst ctx llmod env inst =
  match inst with
  | Lir.Let ((name, _) as place, v) ->
     let {ll; sto} = solve place env ctx in
     Printf.printf "LET %s: %s\n" name (show_storage_t sto);
     let llvalue, _ = generate_value ~recv_var:name ctx llmod env v in
     let ret = match sto with
       | StoImm ->
          L.set_value_name name llvalue;
          let ev = {ll = llvalue; sto = StoImm} in
          let next_env = Map.add name ev env in
          Some (llvalue, next_env)
       | StoRet
       | StoStack ->
          Some (llvalue, env)
       | StoIgnore ->
          None
     in
     ret

  | Lir.Assign ((name, opt_index) as place, v) ->
     Printf.printf "ASSIGN %s.%s\n" name (Option.map_default string_of_int "" opt_index);
     let {ll = recv_ll; sto = recv_sto} = solve place env ctx in
     let (expr_ll, expr_sto) = generate_value ~recv_var:name ctx llmod env v in
     let expr_ty = v.ty in
     let ret = match recv_sto with
       | StoImm ->
          failwith "[ICE] assign a value to an immutable location"
       | StoRet
       | StoStack ->
          let st =
            match expr_sto with
            | StoImm ->
               L.build_store expr_ll recv_ll ctx.llbuilder
            | StoStack when is_stack_type expr_ty ->
               let size = 4 in (* TODO: fix *)
               let align = 4 in (* TODO: fix *)
               let is_volative = false in (* TODO: fix *)
               Intrinsics.memcpy ctx llmod recv_ll expr_ll size align is_volative
            | StoStack ->
               let llv = L.build_load expr_ll "" ctx.llbuilder in
               L.build_store llv recv_ll ctx.llbuilder
            | _ ->
               failwith "[ICE] unexpected expr value"
          in
          Some (st, env)
       | StoIgnore ->
          None
     in
     ret

  | Lir.Nop ->
     None

and generater_terminator ctx m env bb_env terminator =
  match terminator with
  | Lir.Jump index ->
     let next_llbb = Map.find index bb_env in
     L.build_br next_llbb ctx.llbuilder

  | Lir.Cond (cond, then_index, else_index) ->
     let {ll; sto} = solve cond env ctx in
     let llcond = ll in
     let then_llbb = Map.find then_index bb_env in
     let else_llbb = Map.find else_index bb_env in
     L.build_cond_br llcond then_llbb else_llbb ctx.llbuilder

  | Lir.Ret e ->
     let {ll; sto} = solve e env ctx in
     let term = match (ll, sto) with
       | (llv, StoImm) ->
          L.build_ret llv ctx.llbuilder
       | (llv, StoStack) ->
          let llv = L.build_load llv "" ctx.llbuilder in
          L.build_ret llv ctx.llbuilder
       | (_, StoIgnore)
       | (_, StoRet) ->
          L.build_ret_void ctx.llbuilder
     in
     term

let show m =
  let {llmodule} = m in
  L.string_of_llmodule llmodule

let validate m =
  let {llmodule} = m in
  Llvm_analysis.verify_module llmodule

let emit_file ctx m out_path =
  let ll = show m in
  File.with_file_out out_path (fun f -> IO.write_string f ll)
