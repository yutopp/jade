(*
 * Copyright yutopp 2017 - 2018.
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

type t = {
  (* input filepaths for compilation *)
  mutable input_files                       : string list;
  (* output filepath *)
  mutable output_file                       : string option;
  (* if true, do only compilation *)
  mutable compile_only                      : bool;
}

let default () =
  {
    input_files = [];
    output_file = None;
    compile_only = false;
  }

let usage_msg = "Usage: rillirc <options> [filenames]\n"

let show_version_and_exit () =
  Printf.printf "rillirc %s\n%!" "";
  exit 0

let speclist co =
  [
    ("-o",
     Arg.String (fun s -> co.output_file <- Some s),
     "<path> specify an output file name");
    ("-c",
     Arg.Unit (fun b -> co.compile_only <- true),
     "");
  ]

let validated co =
  co

let parse () =
  let co = default () in
  Arg.parse (speclist co |> Arg.align)
            (fun s -> co.input_files <- s :: co.input_files)
            usage_msg;
  validated co
