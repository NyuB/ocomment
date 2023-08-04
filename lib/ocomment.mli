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
  ; lines : hash list
  ; hash : hash
  }

val scan_ocomments : ocomment_settings -> string list -> ocomment list
val valid_lines : string list -> string -> validation
