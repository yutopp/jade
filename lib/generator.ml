(*
 * Copyright yutopp 2018 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

let fresh_id =
  let count = ref 0 in
  let gen () =
    let n = !count in
    let () = incr count in
    Printf.sprintf "__tmp_%d" n
  in
  gen
