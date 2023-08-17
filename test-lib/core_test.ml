open Ocomment.Core

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

let example_nested_comments () =
  let lines =
    [ "name = 'Bob'"
    ; "# ocm start"
    ; "age = 22"
    ; "# ocm start"
    ; "inner = 42"
    ; "# ocm end INNER"
    ; "# ocm end OUTER"
    ; "print(name, 'is', age, 'years')"
    ]
  and settings = { start_prefix = "# ocm start"; end_prefix = "# ocm end" } in
  Alcotest.(check (list testable_ocomment))
    "Expected two ocomments"
    [ { header_line_number = 1
      ; footer_line_number = 6
      ; lines = [ "age = 22"; "inner = 42" ]
      ; hash = "OUTER"
      }
    ; { header_line_number = 3
      ; footer_line_number = 5
      ; lines = [ "inner = 42" ]
      ; hash = "INNER"
      }
    ]
    (scan_ocomments settings lines)
;;

let example_correct_nested_comments () =
  let lines_with_nested =
    [ "name = 'Bob'"
    ; "# ocm start"
    ; "age = 22"
    ; "# ocm start"
    ; "inner = 42"
    ; "# ocm end INNER"
    ; "# ocm end OUTER"
    ; "print(name, 'is', age, 'years')"
    ]
  and lines_without_nested =
    [ "name = 'Bob'"
    ; "# ocm start"
    ; "age = 22"
    ; "inner = 42"
    ; "# ocm end OUTER"
    ; "print(name, 'is', age, 'years')"
    ]
  and settings = { start_prefix = "# ocm start"; end_prefix = "# ocm end" } in
  let expected_outer_hash = "8a1c4ea751af26d96acbe89f438d968c" in
  Alcotest.(check (list string))
    "Expected nested hash correction"
    [ "name = 'Bob'"
    ; "# ocm start"
    ; "age = 22"
    ; "# ocm start"
    ; "inner = 42"
    ; "# ocm end 4445c4331288d68f75d3992afe26f692"
    ; "# ocm end " ^ expected_outer_hash
    ; "print(name, 'is', age, 'years')"
    ]
    (correct settings lines_with_nested);
  Alcotest.(check (list string))
    "Expected outer hash to be the same when removing inner comment"
    [ "name = 'Bob'"
    ; "# ocm start"
    ; "age = 22"
    ; "inner = 42"
    ; "# ocm end " ^ expected_outer_hash
    ; "print(name, 'is', age, 'years')"
    ]
    (correct settings lines_without_nested)
;;

let () =
  Alcotest.run
    "Ocomment"
    [ ( "Scan"
      , [ "No comment", `Quick, example_no_comments
        ; "One comment no hash", `Quick, example_one_comment_no_hash
        ; "One comment with hash", `Quick, example_one_comment_with_hash
        ; "Comment starting mid file", `Quick, example_one_comment_starting_mid_file
        ; "Nested comments", `Quick, example_nested_comments
        ] )
    ; ( "Correct"
      , [ "Empty hash", `Quick, example_correct_empty_hash
        ; "Wrong hash", `Quick, example_correct_wrong_hash
        ; "Nested", `Quick, example_correct_nested_comments
        ] )
    ]
;;