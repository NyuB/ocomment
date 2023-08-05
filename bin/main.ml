let read_file_lines filename =
  let input = open_in filename in
  try
    let rec seq () =
      match In_channel.input_line input with
      | None -> Seq.Nil
      | Some l -> Seq.Cons (l, seq)
    in
    List.of_seq seq
  with
  | e ->
    close_in input;
    raise e
;;

(* #>> *)
let () =
  let filename = Sys.argv.(1)
  and start_prefix = Sys.argv.(2)
  and end_prefix = Sys.argv.(3) in
  print_endline (Printf.sprintf "%s %s %s" filename start_prefix end_prefix);
  let lines = read_file_lines filename in
  let corrected = Ocomment.correction { start_prefix; end_prefix } lines in
  List.iter print_endline corrected
;;
(* #<<
     *)
