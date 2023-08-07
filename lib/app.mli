type markers =
  { start_marker : string
  ; end_marker : string
  }
[@@deriving sexp]

type markers_mapping = (string * markers) list [@@deriving sexp]

type settings =
  { promotion_dir : string
  ; includes : markers_mapping
  }
[@@deriving sexp]

val core_markers : markers -> Core.markers

type main_result = (int, string) result

val check_only : settings_file:string -> files:string list -> main_result
val check_with_promotion : settings_file:string -> files:string list -> main_result
