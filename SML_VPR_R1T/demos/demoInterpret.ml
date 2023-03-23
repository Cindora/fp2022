(** Copyright 2022-2023, Nikita Olkhovsky *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

let () = SML_VPR_lib.Interpreter.interpret (Stdio.In_channel.input_all stdin)
