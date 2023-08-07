type file_type =
  | Directory of string * string array
  | File of string
  | DoesNotExist

val file_type : string -> file_type
val rm : string -> unit
val mkdirp : string -> unit
