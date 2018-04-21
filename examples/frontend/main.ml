(*
 * Copyright yutopp 2017 - 2018.
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open Batteries

module Mir = Jade.Mir

module Compiler = struct
  module PhaseError = struct
    type t =
      | FrontError of Error.t
      | BackError of Jade.Error.t

    let to_msg phase_err =
      match phase_err with
      | FrontError err ->
         Error_msg.to_msg err
      | BackError err ->
         Jade.Error_msg.to_msg err
  end

  type t = {
    diagnostics: PhaseError.t list;
  }

  let is_succeeded result =
    List.length result.diagnostics = 0

  exception Diagnostics of PhaseError.t list

  let rec compile_file in_path out_path =
    try  compile_file' in_path out_path with
    | Diagnostics errors ->
       {
         diagnostics = errors
       }

  and compile_file' in_path out_path =
    Printf.printf "compile_file %s -> %s\n" in_path out_path;
    let ctx = Context.empty () in

    (* parsing *)
    let res = Syntax.make_ast_from_file ctx in_path in
    let tree = match res with
      | Some tree -> tree
      | None ->
         let errors = Context.get_errors ctx in
         raise (Diagnostics (errors |> List.map (fun e -> PhaseError.FrontError e)))
    in
    let () = tree |> Ast.show |> Printf.printf "TREE:\n%s\n" in

    (* typings *)
    let env = Env.empty () in
    let (typed_tree, env) = Typing.generate ctx env tree in
    let () = typed_tree |> T_ast.show |> Printf.printf "TYPED TREE:\n%s\n" in
    let _ = match Context.get_errors ctx with
      | [] -> ()
      | errors ->
         let errors = Context.get_errors ctx in
         raise (Diagnostics (errors |> List.map (fun e -> PhaseError.FrontError e)))
    in

    (* build Jade module *)
    let mm = Builder.build typed_tree in

    let backend_ctx = Jade.Backend.make_context () in
    let m = Jade.Backend.generate backend_ctx mm in
    let () = m |> Jade.Backend.show |> Printf.printf "LLVM:\n%s\n" in
    let () = Jade.Backend.validate m |> Option.may (Printf.printf "%s\n") in

    let () = Jade.Backend.emit_file backend_ctx m out_path in
    {
      diagnostics = []
    }
end

let build co =
  match co.Args.compile_only with
  | true ->
     let in_path = match co.Args.input_files with
       | [p] -> p
       | _   -> failwith "Expect: single file"
     in
     let out_path =
       let p = co.Args.output_file |> Option.default in_path in
       Filetype.convert_path (Filetype.Llvm L_asm) p
     in
     let result = Compiler.compile_file in_path out_path in
     [result]

  | false ->
     let inout_paths =
       co.Args.input_files
       |> List.map (fun in_path ->
                    (in_path, Filetype.convert_path (Filetype.Obj) in_path)
                   )
     in
     let results =
       inout_paths
       |> List.map (fun (in_path, out_path) -> Compiler.compile_file in_path out_path)
     in
     results

let () =
  Printexc.record_backtrace true;

  let co = Args.parse () in
  let results = build co in
  match List.for_all Compiler.is_succeeded results with
  | true ->
     exit 0
  | false ->
     List.iter (fun result ->
                Printf.printf "Diagnostics: %d" (List.length result.Compiler.diagnostics);
                List.iter (fun diagnostics ->
                           Printf.printf "%s\n" (Compiler.PhaseError.to_msg diagnostics)
                          ) result.Compiler.diagnostics
               ) (results |> List.filter (Compiler.is_succeeded %> not));
     exit 1
