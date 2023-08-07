type file_type =
  | Directory of string * string array
  | File of string
  | DoesNotExist

let file_type f =
  if not @@ Sys.file_exists f
  then DoesNotExist
  else if Sys.is_directory f
  then Directory (f, Sys.readdir f |> Array.map (Filename.concat f))
  else File f
;;

let rec rm path =
  match file_type path with
  | Directory (d, childs) ->
    Array.iter rm childs;
    Sys.rmdir d
  | File f -> Sys.remove f
  | DoesNotExist -> ()
;;

let rec mkdirp f =
  match file_type f with
  | Directory _ -> ()
  | File _ ->
    failwith (Printf.sprintf "< %s > is a file and cannot serve as parent directory" f)
  | DoesNotExist ->
    let parent = Filename.dirname f in
    mkdirp parent;
    Sys.mkdir f 0o755
;;
