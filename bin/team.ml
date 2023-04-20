(*
 * Copyright (c) 2022 Thibaut Mattio <thibaut.mattio@gmail.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Cmdliner

let lint () conf year week_range admin_dir =
  let admin_dir =
    match admin_dir with
    | Some x -> x
    | None -> Option.get (Conf.admin_dir conf)
  in
  let week_range =
    match week_range with
    | Some weeks -> weeks
    | None -> failwith "Need to specify week range."
  in
  let lhe, rhe =
    match String.split_on_char '.' week_range with
    | [ lhe; rhe ] -> (lhe, rhe)
    | _ -> failwith "Couldn't decode week range."
  in
  let lhe = int_of_string lhe in
  let rhe = int_of_string rhe in
  let weeks = List.init (rhe - lhe + 1) (fun x -> lhe + x) in
  let teams = Conf.teams conf in
  let lint_report = Okra.Team.lint admin_dir ~year ~weeks teams in
  Format.printf "%a" Okra.Team.pp_lint_report lint_report

let aggregate () conf year week_range admin_dir =
  let admin_dir =
    match admin_dir with
    | Some x -> x
    | None -> Option.get (Conf.admin_dir conf)
  in
  let okr_db =
    match Conf.okr_db conf with
    | None -> None
    | Some f -> Some (Okra.Masterdb.load_csv f)
  in
  let teams = Conf.teams conf in
  let weeks =
    match String.split_on_char '.' (Option.get week_range) with
    | [ from; to_ ] ->
        let from = int_of_string from in
        let to_ = int_of_string to_ in
        List.init (to_ - from + 1) (fun x -> from + x)
    | [ x ] -> [ int_of_string x ]
    | _ -> failwith "failed to parse weeks argument" (* FIXME: better message *)
  in
  let report = Okra.Team.aggregate ?okr_db admin_dir ~year ~weeks teams in
  let pp =
    Okra.Report.pp ~show_time:true ~show_time_calc:false ~show_engineers:true
  in
  Okra.Printer.to_channel stdout pp report

(* Commands *)
let admin_dir =
  let doc = "Path of the admin repository directory." in
  let env = Cmd.Env.info "OKRA_ADMIN_DIR" in
  Arg.(
    value
    & opt (some dir) None
    & info [ "C"; "admin-dir" ] ~docv:"ADMIN_DIR" ~env ~doc)

let weeks =
  let doc = "The week range to consider reports for." in
  Arg.(
    value & opt (some string) None & info [ "W"; "weeks" ] ~docv:"WEEKS" ~doc)

let year =
  let doc = "The year to consider reports for." in
  Arg.(value & opt int 2023 & info [ "Y"; "year" ] ~docv:"YEAR" ~doc)

let lint_cmd =
  let doc = "Lint reports for a team." in
  let info = Cmd.info "lint" ~doc in
  Cmd.v info
    Term.(const lint $ Common.setup () $ Common.conf $ year $ weeks $ admin_dir)

let aggregate_cmd =
  let doc = "Aggregate reports for a team." in
  let info = Cmd.info "aggregate" ~doc in
  Cmd.v info
    Term.(
      const aggregate $ Common.setup () $ Common.conf $ year $ weeks $ admin_dir)

let cmd =
  let doc = "Work on multiple reports for a team" in
  let info = Cmd.info "team" ~doc in
  Cmd.group info [ aggregate_cmd; lint_cmd ]
