open Ocomment

let testable_ocomment =
  Alcotest.testable
    (fun pp o ->
      Format.pp_print_string
        pp
        (Printf.sprintf
           " { header_line_number = %d; footer_line_number = %d; hash = '%s'; lines = [ \
            %s ] }"
           o.header_line_number
           o.footer_line_number
           o.hash
           (String.concat "; " (List.map (fun s -> "'" ^ s ^ "'") o.lines))))
    ( = )
;;

let example_empty_hash_invalid () =
  let lines =
    [ "// sum the lengths of each string in stringList"
    ; "var i = 0;"
    ; "for(var s in stringList) {"
    ; "  i += s.length();"
    ; "}"
    ]
  in
  match valid_lines lines "" with
  | Valid -> Alcotest.fail "Expected invalid for empty hash"
  | Invalid { actual; current } ->
    Alcotest.(check string) "Expected empty string as current hash" "" current;
    Alcotest.(check string)
      "Expected lines md5 as actual hash"
      "3a68fb7e1e0be4d1b3a5be48769ff71a"
      actual
;;

let example_line_break_matter () =
  let lines_with_breaks =
    [ "// sum the lengths of each string in stringList"
    ; "var i = 0;"
    ; "for(var s in stringList) {"
    ; "  i += s.length();"
    ; "}"
    ]
  and lines_with_less_breaks =
    [ "// sum the lengths of each string in stringList"
    ; "var i = 0;"
    ; "for(var s in stringList) {"
    ; "  i += s.length();}"
    ]
  in
  match valid_lines lines_with_breaks "", valid_lines lines_with_less_breaks "" with
  | Invalid { actual; _ }, Invalid { actual = actual_nobreak; _ } ->
    Alcotest.(check string)
      "Expected lines md5 as actual hash"
      "3a68fb7e1e0be4d1b3a5be48769ff71a"
      actual;
    Alcotest.(check string)
      "Expected lines md5 as actual hash"
      "6c7dce90f901aae0eb555819f863c3b0"
      actual_nobreak
  | _ -> Alcotest.fail "Expected invalid for empty hash"
;;

let example_no_comments () =
  let lines = [ "name = 'Bob'"; "age = 22"; "print(name, 'is', age, 'years')" ]
  and settings = { start_prefix = "# ocm start"; end_prefix = "# ocm end" } in
  Alcotest.(check (list testable_ocomment))
    "Expected empty ocomment list for lines without ocomment"
    []
    (scan_ocomments settings lines)
;;

let example_one_comment_no_hash () =
  let lines =
    [ "# ocm start"
    ; "name = 'Bob'"
    ; "age = 22"
    ; "# ocm end"
    ; "print(name, 'is', age, 'years')"
    ]
  and settings = { start_prefix = "# ocm start"; end_prefix = "# ocm end" } in
  Alcotest.(check (list testable_ocomment))
    "Expected one ocomment"
    [ { header_line_number = 0
      ; footer_line_number = 3
      ; lines = [ "name = 'Bob'"; "age = 22" ]
      ; hash = ""
      }
    ]
    (scan_ocomments settings lines)
;;

let example_one_comment_with_hash () =
  let lines =
    [ "# ocm start"
    ; "name = 'Bob'"
    ; "age = 22"
    ; "# ocm end MD5_STRING"
    ; "print(name, 'is', age, 'years')"
    ]
  and settings = { start_prefix = "# ocm start"; end_prefix = "# ocm end" } in
  Alcotest.(check (list testable_ocomment))
    "Expected one ocomment"
    [ { header_line_number = 0
      ; footer_line_number = 3
      ; lines = [ "name = 'Bob'"; "age = 22" ]
      ; hash = "MD5_STRING"
      }
    ]
    (scan_ocomments settings lines)
;;

let () =
  Alcotest.run
    "Ocomment"
    [ ( "Validation"
      , [ "Empty hash", `Quick, example_empty_hash_invalid
        ; "Line breaks", `Quick, example_line_break_matter
        ] )
    ; ( "Scan"
      , [ "No comment", `Quick, example_no_comments
        ; "One comment no hash", `Quick, example_one_comment_no_hash
        ; "One comment with hash", `Quick, example_one_comment_with_hash
        ] )
    ]
;;
