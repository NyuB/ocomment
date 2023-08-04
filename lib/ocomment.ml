type ocomment_settings =
  { prefix : string
  ; suffix : string
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
  | Lines of (ocomment list * string list)
  | Comments of ocomment list

let scan_ocomments (settings : ocomment_settings) (lines : string list) : ocomment list =
  let scan =
    List.fold_left
      (fun acc line ->
        match acc, line with
        | Comments l, header
          when String.starts_with ~prefix:settings.prefix (String.trim header) ->
          Lines (l, [])
        | Comments l, _ -> Comments l
        | Lines (ol, ll), footer
          when String.starts_with ~prefix:settings.suffix (String.trim footer) ->
          Comments
            ({ header_line_number = 0
             ; hash = hash_of_footer settings.suffix footer
             ; lines = List.rev ll
             }
             :: ol)
        | Lines (ol, ll), l -> Lines (ol, l :: ll))
      (Comments [])
      lines
  in
  match scan with
  | Lines (ol, _) | Comments ol -> ol
;;

let valid_lines lines hash =
  let all_lines = String.concat "\n" lines in
  let actual = Digest.string all_lines |> Digest.to_hex in
  if String.equal actual hash then Valid else Invalid { actual; current = hash }
;;
