let help =
  {|
  Usage: ocomment <settings_file> filepath_1 filepath_2 ... filepath_n
    Where <settings_file> should follow this structure:
      ((promotion_dir test/resources/.promote)
      (includes (
      (.java ((start_marker "// <#") (end_marker "// #>")))
      (.py ((start_marker "# ocomment start") (end_marker "# ocomment end")))
      (...)
      (.<file_extension> ((start_marker "start?") (end_marker "end?"))))))
  
  Examples:
  > Validate one file
  ./ocomment .ocomment src/main/com/app/Hello.java
  
  > Validate all .py files in a 'src' folder
  ./ocomment .ocomment src/**/*.py
  NB : If no .py extension markers are configured in the .ocomment this would have no effect
  
  > Validate all files modified since last commit
  ./ocomment .ocomment $(git diff --name-only)
  
  > Validate all files modified between the last two commits in a given folder
  ./ocomment .ocomment $(git diff --name-only HEAD HEAD~ src/)
|}
;;

let () =
  let args = Array.sub Sys.argv 1 (Array.length Sys.argv - 1) in
  let check_only = Array.mem "--check-only" args in
  let actual_args = Array.to_list args |> List.filter (( <> ) "--check-only") in
  if List.length actual_args < 1
  then (
    print_endline "Expected a settings file as first argument";
    print_endline help;
    exit 1)
  else if List.hd actual_args = "--help" || List.hd actual_args = "-h"
  then (
    print_endline help;
    exit 0)
  else (
    let settings_file = List.hd actual_args in
    let action =
      if check_only then Ocomment.App.check_only else Ocomment.App.check_with_promotion
    in
    let files = List.tl actual_args in
    match action ~settings_file ~files with
    | Ok n -> exit n
    | Error e -> failwith e)
;;
