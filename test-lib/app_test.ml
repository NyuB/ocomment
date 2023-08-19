open Ocomment.App
open Ocomment.Core
open Ocomment.Fs

let get_free_temp_name () =
  let id = ref (Random.int32 Int32.max_int) in
  let aux_name () =
    Printf.sprintf "%s%s%ld" (Filename.get_temp_dir_name ()) Filename.dir_sep !id
  in
  let base_path = ref (aux_name ()) in
  while Sys.file_exists !base_path do
    id := Random.int32 Int32.max_int;
    base_path := aux_name ()
  done;
  !base_path
;;

let with_temp_dir exec =
  let base_path = get_free_temp_name () in
  Sys.mkdir base_path 0o755;
  Fun.protect ~finally:(fun () -> rm base_path) (fun () -> exec base_path)
;;

let settings_content settings = Sexplib.Sexp.to_string (sexp_of_settings settings)

let write_content filename content =
  let oc = open_out filename in
  output_string oc content;
  close_out oc
;;

let read_lines filename =
  let ic = open_in filename in
  let rec seq () =
    match In_channel.input_line ic with
    | None -> Seq.Nil
    | Some l -> Seq.Cons (l, seq)
  in
  let res = List.of_seq seq in
  close_in ic;
  res
;;

let correction_with_sources temp settings sources check exec =
  let full_paths =
    List.map (fun (src_file, content) -> Filename.concat temp src_file, content) sources
  in
  List.iter
    (fun (full_path, content) ->
      mkdirp (Filename.dirname full_path);
      write_content full_path content)
    full_paths;
  let settings_file = Filename.concat temp ".ocomment" in
  write_content settings_file (settings_content settings);
  let res = check ~settings_file ~files:(List.map (fun (f, _) -> f) full_paths) in
  exec res
;;

let example_generate_promote_file () =
  with_temp_dir
  @@ fun temp ->
  let promotion_dir = Filename.concat temp "promote" in
  let settings =
    { promotion_dir
    ; includes = [ ".kt", { start_marker = "// ocm start"; end_marker = "// ocm end" } ]
    }
  in
  correction_with_sources
    temp
    settings
    [ ( "src/Example.kt"
      , {|fun test() {
    // ocm start
    // magic number
    val i = 0
    // ocm end
    println(i)
  }|}
      )
    ]
    check_with_promotion
  @@ fun result ->
  Alcotest.(check (result int string))
    "Expected success with error code 1 correction when correction required"
    (Ok 1)
    result;
  let expected_promote_file =
    String.concat Filename.dir_sep [ promotion_dir; temp; "src"; "Example.kt.promote" ]
  in
  Alcotest.(check bool)
    "Expected promotion file to have been created"
    true
    (Sys.file_exists expected_promote_file);
  let corrected_content = read_lines expected_promote_file in
  let corrected_again =
    Ocomment.Core.correct
      { start_prefix = "// ocm start"; end_prefix = "// ocm end" }
      corrected_content
  in
  Alcotest.(check (list string))
    "Expected no correction applicable to promote candidate"
    corrected_content
    corrected_again
;;

let example_ignore_if_extension_not_in_includes () =
  with_temp_dir
  @@ fun temp ->
  let promotion_dir = Filename.concat temp "promote" in
  let settings =
    { promotion_dir
    ; includes =
        [ ".notkt", { start_marker = "// ocm start"; end_marker = "// ocm end" } ]
    }
  in
  correction_with_sources
    temp
    settings
    [ ( "src/Example.kt"
      , {|fun test() {
    // ocm start
    // magic number
    val i = 0
    // ocm end
    println(i)
  }|}
      )
    ]
    check_with_promotion
  @@ fun result ->
  Alcotest.(check (result int string))
    "Expected success with error code 0 for no correction to apply"
    (Ok 0)
    result;
  let expected_promote_file =
    String.concat Filename.dir_sep [ promotion_dir; temp; "src"; "Example.kt.promote" ]
  in
  Alcotest.(check bool)
    "Expected no promotion file to have been created"
    false
    (Sys.file_exists expected_promote_file)
;;

let example_does_not_generate_promote_file_if_no_marker () =
  with_temp_dir
  @@ fun temp ->
  let promotion_dir = Filename.concat temp "promote" in
  let settings =
    { promotion_dir
    ; includes = [ ".kt", { start_marker = "// ocm start"; end_marker = "// ocm end" } ]
    }
  in
  correction_with_sources
    temp
    settings
    [ ( "src/Example.kt"
      , {|fun test() {
    // magic number
    val i = 0
    println(i)
  }|} )
    ]
    check_with_promotion
  @@ fun result ->
  Alcotest.(check (result int string))
    "Expected success with error code 0 for no correction to apply"
    (Ok 0)
    result;
  let no_promote_file =
    String.concat Filename.dir_sep [ promotion_dir; temp; "src"; "Example.kt.promote" ]
  in
  Alcotest.(check bool)
    "Expected no promotion file"
    false
    (Sys.file_exists no_promote_file)
;;

let example_does_not_generate_promote_file_if_only () =
  with_temp_dir
  @@ fun temp ->
  let promotion_dir = Filename.concat temp "promote" in
  let settings =
    { promotion_dir
    ; includes = [ ".kt", { start_marker = "// ocm start"; end_marker = "// ocm end" } ]
    }
  in
  correction_with_sources
    temp
    settings
    [ ( "src/Example.kt"
      , {|fun test() {
    // ocm start
    // magic number
    val i = 0
    // ocm end
    println(i)
  }|}
      )
    ]
    check_only
  @@ fun result ->
  Alcotest.(check (result int string))
    "Expected success with error code 1 when correction required"
    (Ok 1)
    result;
  let no_promote_file =
    String.concat Filename.dir_sep [ promotion_dir; temp; "src"; "Example.kt.promote" ]
  in
  Alcotest.(check bool)
    "Expected no promotion file"
    false
    (Sys.file_exists no_promote_file)
;;

let () =
  Alcotest.run
    "App"
    [ ( "Check with correction"
      , [ "One file one promote", `Quick, example_generate_promote_file
        ; ( "One file without marker, no promote"
          , `Quick
          , example_does_not_generate_promote_file_if_no_marker )
        ; ( "One file with extension not matching includes"
          , `Quick
          , example_ignore_if_extension_not_in_includes )
        ] )
    ; ( "Check only"
      , [ "No promote file", `Quick, example_does_not_generate_promote_file_if_only ] )
    ]
;;
