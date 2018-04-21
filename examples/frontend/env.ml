(*
 * Copyright yutopp 2017 - 2018.
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open Batteries

module StringMap = Map.String

type 't t = {
  table: 't StringMap.t;
  tenv: 't StringMap.t;
}

let empty () =
  {
    table = StringMap.empty;
    tenv = StringMap.empty;
  }

let add id ty env =
  { env with table = StringMap.add id ty env.table }

let find id env =
  match StringMap.find id env.table with
  | x -> Some x
  | exception Not_found -> None
