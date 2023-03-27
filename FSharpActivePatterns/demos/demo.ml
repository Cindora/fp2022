(** Copyright 2022-2023, Grigory Aseev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open FSharpActivePatterns_lib.Interpret

let () = run (Stdio.In_channel.input_all Caml.stdin)
