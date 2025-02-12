(*
 * Copyright (c) 2021 Magnus Skjegstad <magnus@skjegstad.com>
 * Copyright (c) 2021 Patrick Ferris <pf341@patricoferris.com>
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

type lint_error =
  | Format_error of (int * string) list
  | No_time_found of int option * string
  | Invalid_time of int option * string
  | Multiple_time_entries of int option * string
  | No_work_found of int option * string
  | No_KR_ID_found of int option * string
  | No_project_found of int option * string
  | Not_all_includes of string list

type lint_result = (unit, lint_error) result

let fail_fmt_patterns =
  [
    (Str.regexp ".*\t", "Tab found. Use spaces for indentation (2 preferred).");
    ( Str.regexp ".*-  ",
      "Double space before text after bullet point ('-  text'), this can \
       confuse the parser. Use '- text'" );
    ( Str.regexp "^ -",
      "Single space used for indentation (' - text'). Remove or replace by 2 \
       or more spaces." );
    ( Str.regexp "^[ ]*\\*",
      "* used as bullet point, this can confuse the parser. Only use - as \
       bullet marker." );
    ( Str.regexp "^[ ]*\\+",
      "+ used as bullet point, this can confuse the parser. Only use - as \
       bullet marker." );
    ( Str.regexp "^[ ]+#",
      "Space found before title marker #. Start titles in first column." );
  ]

let pp_error ppf = function
  | Format_error x ->
      let pp_msg ppf (pos, msg) = Fmt.pf ppf "Line %d: %s" pos msg in
      Fmt.pf ppf "@[<v 0>%a@,%d formatting errors found. Parsing aborted.@]"
        (Fmt.list ~sep:Fmt.sp pp_msg)
        x (List.length x)
  | No_time_found (_, s) ->
      Fmt.pf ppf
        "@[<hv 2>In KR %S:@ No time entry found. Each KR must be followed by \
         '- @@... (x days)'@]@,"
        s
  | Invalid_time (_, s) ->
      Fmt.pf ppf
        "@[<hv 2>In KR %S:@ Invalid time entry found. Format is '- @@eng1 (x \
         days), @@eng2 (x days)'@]@,"
        s
  | Multiple_time_entries (_, s) ->
      Fmt.pf ppf
        "@[<hv 2>In KR %S:@ Multiple time entries found. Only one time entry \
         should follow immediately after the KR.@]@,"
        s
  | No_work_found (_, s) ->
      Fmt.pf ppf
        "@[<hv 2>In KR %S:@ No work items found. This may indicate an \
         unreported parsing error. Remove the KR if it is without work.@]@,"
        s
  | No_KR_ID_found (_, s) ->
      Fmt.pf ppf
        "@[<hv 2>In KR %S:@ No KR ID found. KRs should be in the format \"This \
         is a KR (PLAT123)\", where PLAT123 is the KR ID. For KRs that don't \
         have an ID yet, use \"New KR\" and for work without a KR use \"No \
         KR\".@]@,"
        s
  | No_project_found (_, s) ->
      Fmt.pf ppf "@[<hv 2>In KR %S:@ No project found (starting with '#')@]@," s
  | Not_all_includes s ->
      Fmt.pf ppf "Missing includes section: %a\n" Fmt.(list ~sep:comma string) s

let string_of_error = Fmt.to_to_string pp_error

(* Check a single line for formatting errors returning
   a list of error messages with the position *)
let check_line line pos =
  List.fold_left
    (fun acc (regexp, msg) ->
      if Str.string_match regexp line 0 then (pos, msg) :: acc else acc)
    [] fail_fmt_patterns

let grep_n s lines =
  let re = Str.regexp_case_fold (".*" ^ Str.quote s ^ ".*") in
  List.find_map
    (fun (i, line) -> if Str.string_match re line 0 then Some i else None)
    lines

(* Parse document as a string to check for aggregation errors
   (assumes no formatting errors) *)
let check_document ~include_sections ~ignore_sections s =
  let lines =
    String.split_on_char '\n' s |> List.mapi (fun i s -> (i + 1, s))
  in
  try
    let md = Omd.of_string s in
    let okrs = Parser.of_markdown ~include_sections ~ignore_sections md in
    let _report = Report.of_krs okrs in
    Ok ()
  with
  | Parser.No_time_found s -> Error (No_time_found (grep_n s lines, s))
  | Parser.Invalid_time s -> Error (Invalid_time (grep_n s lines, s))
  | Parser.Multiple_time_entries s ->
      Error (Multiple_time_entries (grep_n s lines, s))
  | Parser.No_work_found s -> Error (No_work_found (grep_n s lines, s))
  | Parser.No_KR_ID_found s -> Error (No_KR_ID_found (grep_n s lines, s))
  | Parser.No_project_found s -> Error (No_project_found (grep_n s lines, s))
  | Parser.Not_all_includes_accounted_for s -> Error (Not_all_includes s)

let document_ok ~include_sections ~ignore_sections ~format_errors s =
  if !format_errors <> [] then
    Error
      (Format_error
         (List.sort (fun (x, _) (y, _) -> compare x y) !format_errors))
  else check_document ~include_sections ~ignore_sections s

let lint_string_list ?(include_sections = []) ?(ignore_sections = []) lines =
  let format_errors = ref [] in
  let rec check_and_read buf pos = function
    | [] -> Buffer.contents buf
    | line :: rest ->
        format_errors := check_line line pos @ !format_errors;
        Buffer.add_string buf line;
        Buffer.add_string buf "\n";
        check_and_read buf (pos + 1) rest
  in
  let s = check_and_read (Buffer.create 1024) 1 lines in
  document_ok ~include_sections ~ignore_sections ~format_errors s

let lint ?(include_sections = []) ?(ignore_sections = []) ic =
  let format_errors = ref [] in
  let rec check_and_read buf ic pos =
    try
      let line = input_line ic in
      format_errors := check_line line pos @ !format_errors;
      Buffer.add_string buf line;
      Buffer.add_string buf "\n";
      check_and_read buf ic (pos + 1)
    with
    | End_of_file -> Buffer.contents buf
    | e -> raise e
  in
  let s = check_and_read (Buffer.create 1024) ic 1 in
  document_ok ~include_sections ~ignore_sections ~format_errors s

let short_messages_of_error file_name =
  let short_message line_number msg =
    [ Printf.sprintf "%s:%d:%s" file_name line_number msg ]
  in
  let short_messagef line_number_opt fmt =
    let line_number = Option.value ~default:1 line_number_opt in
    Printf.ksprintf (short_message line_number) fmt
  in
  function
  | Format_error errs ->
      List.concat_map
        (fun (line_number, message) -> short_message line_number message)
        errs
  | No_time_found (line_number, kr) ->
      short_messagef line_number "No time found in %S" kr
  | Invalid_time (line_number, kr) ->
      short_messagef line_number "Invalid time in %S" kr
  | Multiple_time_entries (line_number, kr) ->
      short_messagef line_number "Multiple time entries for %S" kr
  | No_work_found (line_number, kr) ->
      short_messagef line_number "No work found for %S" kr
  | No_KR_ID_found (line_number, kr) ->
      short_messagef line_number "No KR ID found for %S" kr
  | No_project_found (line_number, kr) ->
      short_messagef line_number "No project found for %S" kr
  | Not_all_includes l ->
      short_messagef None "Missing includes section: %s" (String.concat ", " l)
