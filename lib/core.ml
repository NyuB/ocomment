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

type lines_scan =
  { completed : ocomment list
  ; nested : ocomment list
  ; current : ocomment
  ; lines_count : int
  }

type scan =
  | In_Ocomment of lines_scan
  | Not_In_Ocomment of ocomment list * int

let add_line_to_comment comment l = { comment with lines = l :: comment.lines }

let add_line_to_current_comment { completed; nested; current; lines_count } l =
  In_Ocomment
    { completed
    ; nested
    ; current = add_line_to_comment current l
    ; lines_count = lines_count + 1
    }
;;

let end_current_comment { completed; nested; current; lines_count } hash =
  let lines = List.rev current.lines in
  let inc_lines_count = lines_count + 1 in
  let completed_comment =
    { header_line_number = current.header_line_number
    ; footer_line_number = lines_count
    ; hash
    ; lines
    }
  in
  match nested with
  | [] -> Not_In_Ocomment (completed_comment :: completed, inc_lines_count)
  | outer :: tail ->
    In_Ocomment
      { completed = completed_comment :: completed
      ; nested = tail
      ; current = { outer with lines = List.rev_append lines outer.lines }
      ; lines_count = inc_lines_count
      }
;;

let start_comment completed nb =
  In_Ocomment
    { completed
    ; nested = []
    ; current =
        { lines = []; header_line_number = nb; footer_line_number = nb; hash = "" }
    ; lines_count = nb + 1
    }
;;

let nest_comment { completed; nested; current; lines_count } =
  In_Ocomment
    { completed
    ; nested = current :: nested
    ; current =
        { lines = []
        ; header_line_number = lines_count
        ; footer_line_number = lines_count
        ; hash = ""
        }
    ; lines_count = lines_count + 1
    }
;;

let skip_uncommented_line l nb = Not_In_Ocomment (l, nb + 1)

let scan_ocomments (markers : markers) (lines : string list) : ocomment list =
  let scan =
    List.fold_left
      (fun acc line ->
        match acc, line with
        | Not_In_Ocomment (ol, nb), header
          when String.starts_with ~prefix:markers.start_prefix (String.trim header) ->
          start_comment ol nb
        | Not_In_Ocomment (ol, nb), _ -> skip_uncommented_line ol nb
        | In_Ocomment lines, header
          when String.starts_with ~prefix:markers.start_prefix (String.trim header) ->
          nest_comment lines
        | In_Ocomment lines, footer
          when String.starts_with ~prefix:markers.end_prefix (String.trim footer) ->
          let hash = hash_of_footer markers.end_prefix footer in
          end_current_comment lines hash
        | In_Ocomment lines, l -> add_line_to_current_comment lines l)
      (Not_In_Ocomment ([], 0))
      lines
  in
  (* Ignore unclosed portion and take the list of completed Comments *)
  match scan with
  | In_Ocomment { completed; _ } | Not_In_Ocomment (completed, _) -> completed
;;

let valid_lines lines hash =
  let all_lines = String.concat "\n" lines in
  let actual = Digest.string all_lines |> Digest.to_hex in
  if String.equal actual hash then Valid else Invalid { actual; current = hash }
;;

type correction =
  { original_lines : string list
  ; to_correct : ocomment list
  ; markers : markers
  }

(* Assume [footer] contains [end_prefix] *)
let apply_footer_correction footer end_prefix hash : string =
  let regexp_end_prefix = Str.regexp_string end_prefix in
  let end_line_split = Str.split_delim regexp_end_prefix footer |> Array.of_list in
  let before = end_line_split.(0)
  and after = end_line_split.(1) in
  let after_split =
    match String.split_on_char ' ' after with
    | [] -> [ hash ]
    | "" :: _ :: t | _ :: t -> hash :: t
  in
  before ^ end_prefix ^ " " ^ String.concat " " after_split
;;

let apply_correction { original_lines; to_correct; markers } : string list =
  let mutable_corrected_lines = Array.of_list original_lines in
  List.iter
    (fun o ->
      let current_line = Array.get mutable_corrected_lines o.footer_line_number in
      let corrected = apply_footer_correction current_line markers.end_prefix o.hash in
      Array.set mutable_corrected_lines o.footer_line_number corrected)
    to_correct;
  Array.to_list mutable_corrected_lines
;;

let correction markers lines : correction =
  let ocomments = scan_ocomments markers lines in
  let validation = List.map (fun o -> o, valid_lines o.lines o.hash) ocomments in
  let to_correct =
    List.filter_map
      (fun (o, v) ->
        match v with
        | Valid -> None
        | Invalid { actual; _ } -> Some { o with hash = actual })
      validation
  in
  { original_lines = lines; to_correct; markers }
;;

let correct markers lines = correction markers lines |> apply_correction

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
      correct { start_prefix = "// ocm start"; end_prefix = "// ocm end" } lines
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
        """ ocm >> nested
        print([e if e != "" else "(...)" for e in split_commas(a)])
        # ocm <<
  # ocm <<
  
  if __name__ == "__main__":
    main(sys.argv[1:])
  |}
    in
    let corrected =
      correct { start_prefix = {|""" ocm >>|}; end_prefix = {|# ocm <<|} } lines
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
            """ ocm >> nested
            print([e if e != "" else "(...)" for e in split_commas(a)])
            # ocm << 9fc8c031db466d4a03228a01de4e8426
      # ocm << 43b9c6337abfcb3e7daaebac0736281b

      if __name__ == "__main__":
        main(sys.argv[1:]) |}]
  ;;
end
