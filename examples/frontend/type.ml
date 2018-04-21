(*
 * Copyright yutopp 2017 - 2018.
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

type primitive =
  | Never
  | Unit
  | Int of {bits: int; signed: bool}
[@@deriving show]

type t =
  | Var of int
  | Primitive of primitive
  | Function of t list * t
  | Struct of string * struct_spec

and struct_spec = {
  members: (string, member_spec) Hashtbl.t [@printer fun x _ -> x]
}
and member_spec = {
  index: int;
  ty: t;
}
[@@deriving show]

module Builtin = struct
  let never =
    Primitive (Never)

  let unit =
    Primitive (Unit)

  let i1 =
    Primitive (Int {bits = 1; signed = true})

  let i32 =
    Primitive (Int {bits = 32; signed = true})
end

let as_func ty =
  match ty with
  | Function (params, ret) -> (params, ret)
  | _ -> failwith "Not function type"

let create_struct name =
  let spec = {
    members = Hashtbl.create 0;
  } in
  Struct (name, spec)

let spec_of_struct ty =
  match ty with
  | Struct (_name, spec) ->
     spec
  | _ ->
     failwith "NOT STRUCT" (* TODO: fix *)

let members_of_struct ty =
  let spec = spec_of_struct ty in
  spec.members

let add_member_to_struct ty m_name m_index m_ty =
  let members = members_of_struct ty in
  Hashtbl.add members m_name {index = m_index; ty = m_ty}

let find_member_of_struct ty m_name =
  let members = members_of_struct ty in
  Hashtbl.find_opt members m_name

let fresh =
  let id = ref 0 in
  let f () =
    let next_id = !id in
    incr id;
    Var next_id
  in
  f
