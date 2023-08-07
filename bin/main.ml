let () =
  let args = Array.sub Sys.argv 1 (Array.length Sys.argv - 1) in
  let settings_file = args.(0) in
  let files = Array.sub args 1 (Array.length args - 1) |> Array.to_list in
  match Ocomment.App.check_with_promotion ~settings_file ~files with
  | Ok n -> exit n
  | Error e -> failwith e
;;
