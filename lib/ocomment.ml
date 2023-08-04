type ocomment_settings =
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
  | Lines of (ocomment list * string list * int)
  | Comments of ocomment list * int

let scan_ocomments (settings : ocomment_settings) (lines : string list) : ocomment list =
  let scan =
    List.fold_left
      (fun acc line ->
        match acc, line with
        | Comments (l, nb), header
          when String.starts_with ~prefix:settings.start_prefix (String.trim header) ->
          Lines (l, [], nb + 1)
        | Comments (l, nb), _ -> Comments (l, nb + 1)
        | Lines (ol, ll, nb), footer
          when String.starts_with ~prefix:settings.end_prefix (String.trim footer) ->
          Comments
            ( { header_line_number = 0
              ; footer_line_number = nb
              ; hash = hash_of_footer settings.end_prefix footer
              ; lines = List.rev ll
              }
              :: ol
            , nb + 1 )
        | Lines (ol, ll, nb), l -> Lines (ol, l :: ll, nb + 1))
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
