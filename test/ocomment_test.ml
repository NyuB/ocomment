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

let example_one_comment_starting_mid_file () =
  let lines =
    [ "name = 'Bob'"
    ; "# ocm start"
    ; "age = 22"
    ; "# ocm end MD5_STRING"
    ; "print(name, 'is', age, 'years')"
    ]
  and settings = { start_prefix = "# ocm start"; end_prefix = "# ocm end" } in
  Alcotest.(check (list testable_ocomment))
    "Expected one ocomment"
    [ { header_line_number = 1
      ; footer_line_number = 3
      ; lines = [ "age = 22" ]
      ; hash = "MD5_STRING"
      }
    ]
    (scan_ocomments settings lines)
;;

let example_correct_empty_hash () =
  let lines =
    [ "name = 'Bob'"
    ; "# ocm start"
    ; "age = 22"
    ; "# ocm end"
    ; "print(name, 'is', age, 'years')"
    ]
  and settings = { start_prefix = "# ocm start"; end_prefix = "# ocm end" } in
  Alcotest.(check (list string))
    "Expected empty hash correction"
    [ "name = 'Bob'"
    ; "# ocm start"
    ; "age = 22"
    ; "# ocm end 86029270bfcce9da33ac53db637747c0"
    ; "print(name, 'is', age, 'years')"
    ]
    (correct settings lines)
;;

let example_correct_wrong_hash () =
  let lines =
    [ "name = 'Bob'"
    ; "# ocm start"
    ; "age = 22"
    ; "# ocm end DEFINITELYNOTAVALIDHASH"
    ; "print(name, 'is', age, 'years')"
    ]
  and settings = { start_prefix = "# ocm start"; end_prefix = "# ocm end" } in
  Alcotest.(check (list string))
    "Expected empty hash correction"
    [ "name = 'Bob'"
    ; "# ocm start"
    ; "age = 22"
    ; "# ocm end 86029270bfcce9da33ac53db637747c0"
    ; "print(name, 'is', age, 'years')"
    ]
    (correct settings lines)
;;

let document_swallow_nested_comment () =
  let lines =
    [ "name = 'Bob'"
    ; "# ocm start"
    ; "age = 22"
    ; "# ocm start"
    ; "# ocm end MD5_STRING"
    ; "# ocm end IGNORED"
    ; "print(name, 'is', age, 'years')"
    ]
  and settings = { start_prefix = "# ocm start"; end_prefix = "# ocm end" } in
  Alcotest.(check (list testable_ocomment))
    "Expected one ocomment"
    [ { header_line_number = 1
      ; footer_line_number = 4
      ; lines = [ "age = 22"; "# ocm start" ]
      ; hash = "MD5_STRING"
      }
    ]
    (scan_ocomments settings lines)
;;

let () =
  Alcotest.run
    "Ocomment"
    [ ( "Scan"
      , [ "No comment", `Quick, example_no_comments
        ; "One comment no hash", `Quick, example_one_comment_no_hash
        ; "One comment with hash", `Quick, example_one_comment_with_hash
        ; "Comment starting mid file", `Quick, example_one_comment_starting_mid_file
        ] )
    ; ( "Correct"
      , [ "Empty hash", `Quick, example_correct_empty_hash
        ; "Wrong hash", `Quick, example_correct_wrong_hash
        ] )
    ; ( "Document unexpected behaviour"
      , [ "Swallow nested comment", `Quick, document_swallow_nested_comment ] )
    ]
;;
