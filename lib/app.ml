open Sexplib.Conv

type markers =
  { start_marker : string
  ; end_marker : string
  }
[@@deriving sexp]

let core_markers ({ start_marker; end_marker } : markers) : Core.markers =
  { end_prefix = end_marker; start_prefix = start_marker }
;;

type markers_mapping = (string * markers) list [@@deriving sexp]

type settings =
  { promotion_dir : string
  ; includes : markers_mapping
  }
[@@deriving sexp]

type main_result = (int, string) result

let read_lines filename =
  let ic = open_in filename in
  let rec line_seq () =
    match In_channel.input_line ic with
    | Some l -> Seq.Cons (l, line_seq)
    | None -> Seq.Nil
  in
  let res = List.of_seq line_seq in
  close_in ic;
  res
;;

let write_lines filename lines =
  let oc = open_out filename in
  Out_channel.output_string oc (String.concat "\n" lines);
  close_out oc
;;

let check_with_action ~settings_file ~files action =
  let module StringMap = Map.Make (String) in
  let settings = Sexplib.Sexp.input_sexp (open_in settings_file) |> settings_of_sexp in
  let markers = settings.includes |> List.to_seq |> StringMap.of_seq in
  let at_least_one_correction = ref false in
  List.iter
    (fun f ->
      let ext = Filename.extension f in
      let markers_opt = StringMap.find_opt ext markers in
      Option.iter
        (fun markers ->
          let lines = read_lines f in
          let correction = Core.correction markers lines in
          if List.length correction.to_correct = 0
          then ()
          else (
            at_least_one_correction := true;
            action settings f correction))
        (Option.map core_markers markers_opt))
    files;
  Ok (if !at_least_one_correction then 1 else 0)
;;

let format_ocomment_line_range ocm =
  let open Core in
  Printf.sprintf
    "lines (%d <-> %d)"
    (ocm.header_line_number + 1)
    (ocm.footer_line_number + 1)
;;

let check_only ~settings_file ~files =
  check_with_action ~settings_file ~files (fun _ f correction ->
    let ranges =
      List.map (fun o -> "\t" ^ format_ocomment_line_range o) correction.to_correct
    in
    prerr_endline
      (Printf.sprintf
         "Invalid ocomment found in file '%s'\n%s"
         f
         (String.concat "\n" ranges)))
;;

let check_with_promotion ~settings_file ~files =
  check_with_action ~settings_file ~files (fun settings invalid_file_name correction ->
    let promote_file =
      Filename.concat settings.promotion_dir (invalid_file_name ^ ".promote")
    in
    let () = Fs.mkdirp (Filename.dirname promote_file) in
    write_lines promote_file (Core.apply_correction correction))
;;

module Test = struct
  let%expect_test _ =
    let settings =
      { promotion_dir = ".promote"
      ; includes = [ ".java", { start_marker = "// <#"; end_marker = "// #>" } ]
      }
    in
    print_endline (sexp_of_settings settings |> Sexplib.Sexp.to_string_hum);
    [%expect
      {|
    ((promotion_dir .promote)
     (includes ((.java ((start_marker "// <#") (end_marker "// #>"))))))
     |}]
  ;;
end
