type hash = string

type ocomment =
  { header_line_number : int
  ; footer_line_number : int
  ; lines : string list
  ; hash : hash
  }

type markers =
  { start_prefix : string
  ; end_prefix : string
  }

type correction

val scan_ocomments : markers -> string list -> ocomment list
val correction : markers -> string list -> correction
val apply_correction : correction -> string list
val correct : markers -> string list -> string list
