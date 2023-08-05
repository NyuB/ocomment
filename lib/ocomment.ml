type markers =
  { start_prefix : string
  ; end_prefix : string
  }

type hash = string

type invalid_hash =
  { actual : hash
  ; current : hash
  }

type validation =
  | Invalid of invalid_hash
  | Valid

type ocomment =
  { header_line_number : int
  ; footer_line_number : int
  ; lines : string list
  ; hash : hash
  }

let hash_of_footer footer_prefix footer =
  let prefix_length = String.length footer_prefix
  and header_length = String.length footer in
  if prefix_length >= header_length
  then ""
  else String.sub footer (prefix_length + 1) (header_length - prefix_length - 1)
;;

type scan =
  | Lines of (ocomment list * ocomment * int)
  | Comments of ocomment list * int

let add_line_to_current_comment ol c nb l =
  Lines (ol, { c with lines = l :: c.lines }, nb + 1)
;;

let end_current_comment ol c nb hash =
  Comments
    ( { header_line_number = c.header_line_number
      ; footer_line_number = nb
      ; hash
      ; lines = List.rev c.lines
      }
      :: ol
    , nb + 1 )
;;

let start_comment ol nb =
  Lines
    ( ol
    , { lines = []; header_line_number = nb; footer_line_number = nb; hash = "" }
    , nb + 1 )
;;

let skip_uncommented_line l nb = Comments (l, nb + 1)

let scan_ocomments (settings : markers) (lines : string list) : ocomment list =
  let scan =
    List.fold_left
      (fun acc line ->
        match acc, line with
        | Comments (l, nb), header
          when String.starts_with ~prefix:settings.start_prefix (String.trim header) ->
          start_comment l nb
        | Comments (l, nb), _ -> skip_uncommented_line l nb
        | Lines (ol, c, nb), footer
          when String.starts_with ~prefix:settings.end_prefix (String.trim footer) ->
          let hash = hash_of_footer settings.end_prefix footer in
          end_current_comment ol c nb hash
        | Lines (ol, c, nb), l -> add_line_to_current_comment ol c nb l)
      (Comments ([], 0))
      lines
  in
  match scan with
  | Lines (ol, _, _) | Comments (ol, _) -> ol
;;

let valid_lines lines hash =
  let all_lines = String.concat "\n" lines in
  let actual = Digest.string all_lines |> Digest.to_hex in
  if String.equal actual hash then Valid else Invalid { actual; current = hash }
;;

let before line prefix =
  let index = ref 0 in
  let prefix_len = String.length prefix in
  while not (String.equal (String.sub line !index prefix_len) prefix) do
    index := !index + 1
  done;
  String.sub line 0 !index
;;

let correction settings lines =
  let ocomments = scan_ocomments settings lines in
  let corrections = List.map (fun o -> o, valid_lines o.lines o.hash) ocomments in
  let mutable_corrected_lines = Array.of_list lines in
  List.iter
    (fun (o, v) ->
      match v with
      | Valid -> ()
      | Invalid { actual; _ } ->
        let current_line = Array.get mutable_corrected_lines o.footer_line_number in
        let preserved_indent = before current_line settings.end_prefix in
        Array.set
          mutable_corrected_lines
          o.footer_line_number
          (preserved_indent ^ settings.end_prefix ^ " " ^ actual))
    corrections;
  Array.to_list mutable_corrected_lines
;;

module Test = struct
  let lines_of s = String.split_on_char '\n' s
  let print_lines l = List.iter print_endline l

  let string_of_validation = function
    | Valid -> "Valid"
    | Invalid { actual; current } ->
      Printf.sprintf "Invalid { actual = %s; current = %s }" actual current
  ;;

  let print_validation validation = print_endline @@ string_of_validation validation

  let%expect_test "empty hash is invalid" =
    let lines =
      [ "// sum the lengths of each string in stringList"
      ; "var i = 0;"
      ; "for(var s in stringList) {"
      ; "  i += s.length();"
      ; "}"
      ]
    in
    print_validation @@ valid_lines lines "";
    [%expect {| Invalid { actual = 3a68fb7e1e0be4d1b3a5be48769ff71a; current =  } |}]
  ;;

  let%expect_test "line breaks matter" =
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
    print_validation @@ valid_lines lines_with_breaks "INVALID_HASH";
    [%expect
      {| Invalid { actual = 3a68fb7e1e0be4d1b3a5be48769ff71a; current = INVALID_HASH } |}];
    print_validation @@ valid_lines lines_with_less_breaks "INVALID_HASH";
    [%expect
      {| Invalid { actual = 6c7dce90f901aae0eb555819f863c3b0; current = INVALID_HASH } |}]
  ;;

  let%expect_test "Valid if actual hash == current" =
    let lines =
      [ "// sum the lengths of each string in stringList"
      ; "var i = 0;"
      ; "for(var s in stringList) {"
      ; "  i += s.length();"
      ; "}"
      ]
    in
    print_validation @@ valid_lines lines "3a68fb7e1e0be4d1b3a5be48769ff71a";
    [%expect {| Valid |}]
  ;;

  let%expect_test _ =
    let lines =
      lines_of
        {|
    class Example {
      public static int sum(int a; int b) {
        // ocm start
        // Complicated operation description
        return a + b;
        // ocm end
      }
    }
  |}
    in
    let corrected =
      correction { start_prefix = "// ocm start"; end_prefix = "// ocm end" } lines
    in
    print_lines corrected;
    [%expect
      {|
    class Example {
      public static int sum(int a; int b) {
        // ocm start
        // Complicated operation description
        return a + b;
        // ocm end 259ded6fc990a61e7b65df8f4644e760
      }
    } |}]
  ;;

  let%expect_test _ =
    let lines =
      lines_of
        {|
  import sys

  def split_commas(s: str) -> 'list[str]':
      return s.split(',')
  """ ocm >>
  Split each argument by commas and display items separated by spaces on a separate line
  Replace blank element with (...) in case of starting or ending comma
  """
  def main(args: 'list[str]') -> None:
      for a in args:
          print([e if e != "" else "(...)" for e in split_commas(a)])
  # ocm <<
  
  if __name__ == "__main__":
    main(sys.argv[1:])
  |}
    in
    let corrected =
      correction { start_prefix = {|""" ocm >>|}; end_prefix = {|# ocm <<|} } lines
    in
    print_lines corrected;
    [%expect
      {|
      import sys

      def split_commas(s: str) -> 'list[str]':
          return s.split(',')
      """ ocm >>
      Split each argument by commas and display items separated by spaces on a separate line
      Replace blank element with (...) in case of starting or ending comma
      """
      def main(args: 'list[str]') -> None:
          for a in args:
              print([e if e != "" else "(...)" for e in split_commas(a)])
      # ocm << 0e0dfbf4eca5a8f86fca15f7909b3a24

      if __name__ == "__main__":
        main(sys.argv[1:]) |}]
  ;;
end
