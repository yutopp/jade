(*
 * Copyright yutopp 2017 - 2018.
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

type t = (kind * Loc.t)
and kind =
  | Not_found of string
  | Type_mismatch of {expect: Mir.Type.t; actual: Mir.Type.t}
  | Type_mismatch_struct of {actual: Mir.Type.t}
  | No_member of {ty: Mir.Type.t; name: string}
