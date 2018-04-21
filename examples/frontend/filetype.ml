(*
 * Copyright yutopp 2017 - 2018.
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

type t =
  | Obj
  | Llvm of llvm_kind
  | Executable

and llvm_kind =
  | L_asm
  | L_bit_code

let convert_path t path =
  let path_without_ext = match Filename.chop_extension path with
    | s -> s
    | exception Invalid_argument _ -> path
  in
  let ext = match t with
    | Obj -> ".o"
    | Llvm L_asm -> ".ll"
    | Llvm L_bit_code -> ".bc"
    | Executable -> ".out"
  in
  path_without_ext ^ ext
