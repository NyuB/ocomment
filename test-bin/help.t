Help message and exit code 0 with -h or --help:
  $ ocomment -h
  
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
  

  $ ocomment --help
  
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
  
Help message and exit code 1 for missing first argument:
  $ ocomment
  Expected a settings file as first argument
  
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
  
  [1]
