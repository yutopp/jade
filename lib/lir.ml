(*
 * Copyright yutopp 2017 - 2018.
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open Batteries

module VarsMap = Map.Make(String)

type mut =
  | MutVar
  | MutImm

type basic_block_t = {
  index: basic_block_index_t;
  mutable insts: inst_t Vect.t;
  mutable terminator: terminator_t option;
}
and vars_t = (Mir.Type.t * mut) VarsMap.t
and value_t = {kind: value_kind_t; ty: Mir.Type.t}
and value_kind_t =
  | Function of function_t * vars_t
  | Const of const_value
  | Copy of placeholder
  | Move of placeholder
  | Var of placeholder
  | Call of placeholder * placeholder list
  | Unit
and inst_t =
  | Let of placeholder * value_t
  | Assign of placeholder * value_t
  | Nop
and terminator_t =
  | Jump of basic_block_index_t
  | Cond of placeholder * basic_block_index_t * basic_block_index_t
  | Ret of placeholder
and const_value =
  | ValueInt of int
  | ValueUndef
and basic_block_index_t =
  int
and function_t = {
  name: string;
  params: placeholder list;
  mutable ret_var: value_t option;
  mutable bb_index: basic_block_index_t;
  mutable basic_blocks: basic_block_t Vect.t;
}
and placeholder = string * int option

let string_of_placeholder p =
  let (name, index_opt) = p in
  match index_opt with
  | Some index ->
     Printf.sprintf "%s.%d" name index
  | None ->
     name

let succ bb =
  match bb.terminator with
  | Some (Jump i) -> [i]
  | Some (Cond (_, t, f)) -> [t; f]
  | Some (Ret _) -> []
  | None -> []

let fold ?(start_at=0)
         ?(next=fun bb -> succ bb)
         f acc0 basic_blocks =
  let start_bb = Vect.at basic_blocks start_at in

  let visited = Array.create (Vect.length basic_blocks) false in
  visited.(start_at) <- true;

  let q = Queue.create () in
  Queue.push start_bb.index q;

  let rec collect acc =
    match Queue.is_empty q with
    | true ->
       acc
    | false ->
       let bb_index = Queue.pop q in
       let bb = Vect.at basic_blocks bb_index in

       let acc = f acc bb in

       List.iter
         (fun next_index ->
          if not visited.(next_index) then
            begin
              visited.(next_index) <- true;
              Queue.push next_index q
            end
         ) (next bb);

       collect acc
  in
  collect acc0

let map f basic_blocks =
  let f' acc bb =
    let bb' = f bb in
    Vect.set acc bb.index {bb' with index = bb.index}
  in
  fold f' basic_blocks basic_blocks

type module_t = {
  mutable functions: value_t list;
}

let make_module () =
  {
    functions = []
  }

type context_t = {
  mutable current_module: module_t option;
  mutable current_function: value_t option;
  mutable current_basic_block: basic_block_t option;
}

let make_context () =
  {
    current_module = None;
    current_function = None;
    current_basic_block = None;
  }

let set_current_module ctx mv =
  ctx.current_module <- Some mv;
  ()

let set_current_function ctx fv =
  ctx.current_function <- Some fv;
  ()

let current_function ctx =
  match ctx.current_function with
  | Some {kind = Function (f, _)} -> f
  | Some _ -> failwith "not a function value"
  | None   -> failwith "there is no current function"

let create_function params ty =
  let f = {
    name = "";
    params = params;
    ret_var = None;
    bb_index = 0;
    basic_blocks = Vect.empty;
  } in
  {kind = Function (f, VarsMap.empty); ty}

let set_ret_var_to_function f var =
  let spec = match f with
    | {kind = Function (s, _)} -> s
    | _ -> failwith "[ICE]"
  in
  spec.ret_var <- Some var

let get_ret_var_from_function f =
  let spec = match f with
    | {kind = Function (s, _)} -> s
    | _ -> failwith "[ICE]"
  in
  spec.ret_var

let declare_module () =
  make_module ()

let set_current_bb ctx bb =
  ctx.current_basic_block <- Some bb;
  ()

let append_block ctx name =
  let f = current_function ctx in

  let index = Vect.length f.basic_blocks in
  let bb = {
    index = index;
    insts = Vect.empty;
    terminator = None;
  } in
  f.basic_blocks <- Vect.append bb f.basic_blocks;

  ctx.current_basic_block <- Some bb;
  bb

let current_bb ctx =
  match ctx.current_basic_block with
  | Some bb -> bb
  | None -> failwith "there is no current basic block"

let add_inst bb inst =
  bb.insts <- Vect.append inst bb.insts;
  ()

let set_terminator bb terminator =
  match bb.terminator with
  | None ->
     bb.terminator <- Some terminator
  | Some _ ->
     failwith "Terminator is already set"

let build_let ctx name v =
  let {ty} = v in
  let name = if name = "" then Generator.fresh_id () else name in
  let bb = current_bb ctx in
  let _ = add_inst bb (Let ((name, None), v)) in
  {kind = Var (name, None); ty}

let build_assign ctx (lhs_name, lhs_index) rhs ty =
  let name = if lhs_name = "" then Generator.fresh_id () else lhs_name in
  let bb = current_bb ctx in
  let _ = add_inst bb (Assign ((name, lhs_index), {kind = Var rhs; ty})) in
  {kind = Var (name, None); ty}

let build_call ctx name v =
  match v with
  | {kind = Call _; ty} ->
     let name = if name = "" then Generator.fresh_id () else name in
     let bb = current_bb ctx in
     let _ = add_inst bb (Let ((name, None), v)) in
     {kind = Var (name, None); ty}
  | _ ->
     failwith ""

let build_if ctx cond then_bb else_bb =
  let bb = current_bb ctx in
  let _ = set_terminator bb (Cond (cond, then_bb.index, else_bb.index)) in
  Unit

let build_jump ctx next_bb =
  let bb = current_bb ctx in
  let _ = set_terminator bb (Jump next_bb.index) in
  Unit

let build_ret ctx e =
  let bb = current_bb ctx in
  let _ = set_terminator bb (Ret e) in
  Unit

let rec generate ctx env k_form =
  match k_form with
  | K_normal.{kind = Module {nodes}; loc} ->
     let m = declare_module () in
     let () = set_current_module ctx m in
     let () = List.iter (fun node -> generate' ctx env node |> ignore) nodes in
     m
  | _ ->
     failwith "not supported by toplevel"

and generate' ctx env k_form =
  match k_form with
  | K_normal.{kind = ExprFunc {params; body}; ty; loc} ->
     let fv = create_function params ty in
     let () = set_current_function ctx fv in
     let _entry = append_block ctx "entry" in
     let _ =
       (* reciever for the return value *)
       let ret_ty = Mir.Type.ret_ty_of_func_ty ty in
       let ret_var = build_let ctx "__ret" ({kind = Const ValueUndef; ty = ret_ty}) in
       set_ret_var_to_function fv ret_var
     in
     let _ = generate' ctx env body in
     fv

  | K_normal.{kind = Seq {nodes}; ty; loc} ->
     let base =
       {kind = Unit; ty = Mir.Type.Builtin.never}
     in
     List.fold_left (fun acc n ->
                     generate' ctx env n
                    ) base nodes

  | K_normal.{kind = Let {name = (name, _); expr}; ty; loc} ->
     let expr_v = generate' ctx env expr in
     build_let ctx name expr_v

  | K_normal.{kind = Num n; ty; loc} ->
     {kind = Const (ValueInt n); ty}

  | K_normal.{kind = Unit; ty; loc} ->
     {kind = Const ValueUndef; ty}

  | K_normal.{kind = Undef; ty; loc} ->
     {kind = Const ValueUndef; ty}

  | K_normal.{kind = IfStmt {cond;
                             then_c = then_n;
                             else_c = else_n;
                            }; loc} ->
     let current_bb = current_bb ctx in

     let then_bb = append_block ctx "" in
     let else_bb = append_block ctx "" in
     let term_bb = append_block ctx "" in

     let () = set_current_bb ctx then_bb in
     let _then_v = generate' ctx env then_n in
     let _ = build_jump ctx term_bb in

     let () = set_current_bb ctx else_bb in
     let _else_v = generate' ctx env else_n in
     let _ = build_jump ctx term_bb in

     let () = set_current_bb ctx current_bb in
     let _ = build_if ctx cond then_bb else_bb in

     let () = set_current_bb ctx term_bb in

     {kind = Unit; ty = Mir.Type.Builtin.unit}

  | K_normal.{kind = Return e; loc} ->
     let ret_var = get_ret_var_from_function (ctx.current_function |> Option.get) |> Option.get in
     let {kind = ret_var_kind; ty = ret_var_ty} = ret_var in
     let ret_var_place = match ret_var_kind with
       | Var p -> p
       | _ -> failwith "[ICE]"
     in
     let _ = build_assign ctx ret_var_place e ret_var_ty in
     let _ = build_ret ctx ret_var_place in
     {kind = Unit; ty = Mir.Type.Builtin.unit}

  | K_normal.{kind = Init placeholder; ty; loc} ->
     {kind = Var placeholder; ty}

  | K_normal.{kind = Var placeholder; ty; loc} ->
     {kind = Copy placeholder; ty}

  | K_normal.{kind = Assign {lhs; rhs}; ty; loc} ->
     build_assign ctx lhs rhs ty

  | K_normal.{kind = Call {name; args}; ty; loc} ->
     build_call ctx "" {kind = (Call (name, args)); ty}

  | _ ->
     failwith "not supprted k form"

let rec complete_pass' ctx env ir =
  match ir with
  | {kind = Function (f, v); ty} ->
     let {
       basic_blocks
     } = f in
     let _ = basic_blocks in
     {kind = Function (f, v); ty}
  | _ ->
     failwith ""

let rec replace_copy_move_pass' ctx env ir =
  match ir with
  | {kind = Function (f, vars); ty} ->
     let {
       basic_blocks
     } = f in
     let conv value = match value with
       | {kind = Copy src; ty} -> value
       | _ -> value
     in
     let basic_blocks' =
       map (fun bb ->
            let insts' =
              Vect.foldi
                (fun index acc inst ->
                 let inst' =
                   match inst with
                   | Let (place, value) ->
                      Let (place, value |> conv)
                   | Assign (place, value) ->
                      Assign (place, value |> conv)
                   | Nop ->
                      Nop
               in
               Vect.set acc index inst'
              ) bb.insts bb.insts
          in
          let terminator' =
            bb.terminator
          in
          {bb with insts = insts'; terminator = terminator'}
         ) basic_blocks
    in
    let f' = {
      f with basic_blocks = basic_blocks'
    } in
    {kind = Function (f', vars); ty}
  | _ ->
     failwith "[ICE] replace_copy_move_pass': unexpected ir"

let rec reduce_tmp_vars_pass' _ctx _env ir =
  match ir with
  | {kind = Function (f, vars); ty} ->
     let {
       basic_blocks
     } = f in

     (* collect dupped vars and assign *)
     let beta_m =
       fold (fun acc bb ->
             Vect.fold_left
               (fun bacc inst ->
                match inst with
                | Let ((name, None), {kind = Var (vname, None)}) when not (Map.mem name bacc) ->
                   Map.add name vname bacc
                | Let ((name, None), {kind = Var (vname, None)}) ->
                   failwith (Printf.sprintf "[ICE] dup key: %s -> %s" name vname)
                | Let ((name, Some _), _) ->
                   failwith (Printf.sprintf "[ICE] lhs is indexed:: name=%s" name)
                | _ ->
                   bacc
               ) acc bb.insts
            ) Map.empty basic_blocks
     in

     Map.iter (fun k v ->
               Printf.printf "BETA: %s -> %s\n" k v
              ) beta_m;

     let basic_blocks =
       let rec replace place =
         let (name, index_opt) = place in
         match replace (Map.find name beta_m, index_opt) with
         | place' -> place'
         | exception Not_found -> place
       in
       let replace_value v =
         match v with
         | {kind = Call (callee, args); ty} ->
            {kind = Call (callee |> replace, args |> List.map replace); ty}
         | {kind = Copy place; ty} ->
            {kind = Copy (place |> replace); ty}
         | {kind = Move place; ty} ->
            {kind = Move (place |> replace); ty}
         | _ ->
            v
       in
       let replace_terminator t =
         match t with
         | Cond (c, t, e) ->
            Cond (c |> replace, t, e)
         | Ret n ->
            Ret (n |> replace)
         | Jump _ ->
            t
       in
       map (fun bb ->
            let insts' =
              Vect.foldi
                (fun index acc i ->
                 let i' =
                   match i with
                   (*| Let (name, ({kind = Var _} as value)) ->
                      let new_value = replace_value value in
                      if value <> new_value then
                        Let (name, new_value)
                      else
                        Let (name, value)*)
                   | Let (name, {kind = Var _}) ->
                      Nop
                   | Let (name, value) ->
                      Let (name, value |> replace_value)
                   | Assign (lhs, ({kind = Var n} as rhs)) ->
                      Assign (lhs |> replace, {rhs with kind = Var (n |> replace)})
                   | Assign _ ->
                      failwith ""
                   | Nop ->
                      i
                 in
                 Vect.set acc index i'
                ) bb.insts bb.insts
            in
            let terminator' =
              bb.terminator |> Option.map (replace_terminator)
            in
            {bb with insts = insts'; terminator = terminator'}
           ) basic_blocks
     in

     let assign_m =
       fold (fun acc bb ->
             Vect.fold_left
               (fun aacc inst ->
                match inst with
                | Assign ((lhs_name, lhs_index_opt), {kind = Var (n, None)}) when not (Map.mem n aacc) ->
                   Map.add n (lhs_name, lhs_index_opt) aacc
                | Assign (lhs, rhs) ->
                   failwith "[ICE] not supported"
                | _ ->
                   aacc
               ) acc bb.insts
            ) Map.empty basic_blocks
     in

     Map.iter (fun rhs_name lhs ->
               Printf.printf "ASSIGN: %s <- %s\n" (string_of_placeholder lhs) rhs_name
              ) assign_m;

     let basic_blocks =
       let rec replace place : placeholder =
         let (name, index_opt) = place in
         Printf.printf "placing %s to " (string_of_placeholder place);
         match replace (Map.find name assign_m) with
         | (place', None) ->
            Printf.printf "%s\n" (place');
            (place', index_opt)
         | place' ->
            place'
         | exception Not_found ->
                     Printf.printf "END\n";
                     place
       in
       map (fun bb ->
            let insts' =
              Vect.foldi
                (fun index acc i ->
                 let i' =
                   match i with
                   | Let (place, value) ->
                      let replaced = place |> replace in
                      if place != replaced then
                        match value with
                        | {kind = Const ValueUndef} ->
                           Nop
                        | _ ->
                           Assign (replaced, value)
                      else
                        Let (replaced, value)
                   | Assign _
                   | Nop ->
                      Nop
                 in
                 Vect.set acc index i'
                ) bb.insts bb.insts
            in
            let terminator' =
              bb.terminator
            in
            {bb with insts = insts'; terminator = terminator'}
           ) basic_blocks
     in

     let f' = {
       f with basic_blocks
     } in
     {kind = Function (f', vars); ty}
                   | _ ->
                      failwith ""

let rec collect_stack_pass' ctx env ir =
  match ir with
  | {kind = Function (f, _); ty} ->
     let {
       basic_blocks
     } = f in

     let vars =
       fold (fun macc bb ->
             Vect.fold_left
               (fun m inst ->
                match inst with
                | Let ((name, _), value) when not (VarsMap.mem name m) ->
                   let (ty, mut) = match value with
                     | {kind = Const ValueUndef; ty} ->
                        (ty, MutVar) (* value which will be assigned later *)
                     | {ty} ->
                        (ty, MutImm)
                   in
                   VarsMap.add name (ty, mut) m
                | _ ->
                   m
               ) macc bb.insts
            ) VarsMap.empty basic_blocks
     in

     VarsMap.iter (fun k v ->
                   Printf.printf "VAR: %s -> %s\n" k ""
                  ) vars;

     {kind = Function (f, vars); ty}
  | _ ->
     failwith ""

let rec show ir =
  let buf = Buffer.create 0 in
  let () = show_impl buf 0 ir in
  Buffer.contents buf

and show_value ir_value =
  let buf = Buffer.create 0 in
  let () = show_value_impl buf 0 ir_value in
  Buffer.contents buf

and show_impl buf i m =
  let p ?(i=i) fmt = Format.bprintf buf ("%s" ^^ fmt) (String.repeat " " (i*2)) in
  p "Module\n";
  let {functions} = m in
  List.iter (show_value_impl buf (i+1)) functions

and show_value_impl buf i v =
  let p ?(i=i) fmt = Format.bprintf buf ("%s" ^^ fmt) (String.repeat " " (i*2)) in
  match v with
  | {kind = Function (f, v)} ->
     let {
       name;
       basic_blocks;
     } = f in
     p "Function: %s\n" name;
     Vect.iter (fun bb ->
                let {
                  index;
                  insts;
                  terminator
                } = bb in
                p "BB(%d):\n" index;
                insts |> Vect.iter (show_inst_impl buf (i+1));
                match terminator with
                | Some t ->
                   show_terminator_impl buf (i+1) t
                | None ->
                   p ~i:(i+1) "NO TERMINATOR"
               )
               basic_blocks

  | {kind = Call (callee, args)} ->
     p "call %s(%s)"
       (string_of_placeholder callee)
       (args |> List.map string_of_placeholder |> String.join ",")

  | {kind = Const (ValueInt n)} ->
     p "const int %d" n

  | {kind = Const ValueUndef} ->
     p "const undef"

  | {kind = Copy v} ->
     p "copy %s" (string_of_placeholder v)

  | {kind = Move v} ->
     p "MOVE %s" (string_of_placeholder v)

  | {kind = Var n} ->
     p "%s" (string_of_placeholder n)

  | {kind = Unit} ->
     p "()"

and show_inst_impl buf i inst =
  let p ?(i=i) fmt = Format.bprintf buf ("%s" ^^ fmt) (String.repeat " " (i*2)) in
  match inst with
  | Let (place, value) ->
     let (name, opt_index) = place in
     p "let %s%s: %s = "
       name
       (opt_index |> Option.map_default (Printf.sprintf ".%d") "")
       (Mir.Type.show value.ty);
     show_value_impl buf 0 value;
     Format.bprintf buf (";\n")
  | Assign (place, value) ->
     let (name, opt_index) = place in
     p "%s%s = " name (opt_index |> Option.map_default (Printf.sprintf ".%d") "");
     show_value_impl buf 0 value;
     Format.bprintf buf (";\n")
  | Nop ->
     ()

and show_terminator_impl buf i terminator =
  let p ?(i=i) fmt = Format.bprintf buf ("%s" ^^ fmt) (String.repeat " " (i*2)) in
  match terminator with
  | Jump index ->
     p "JUMP %d\n" index
  | Cond (cond, then_bb_index, else_bb_index) ->
     p "IF %s THEN %d ELSE %d\n" (string_of_placeholder cond) then_bb_index else_bb_index
  | Ret e ->
     p "RET %s\n" (string_of_placeholder e)
