When called with --check-only on invalid files, exit with error code but do not generate promote files and print out invalid files
  $ ocomment --check-only ocomment-settings src/*
  Invalid ocomment found in file 'src/Example.kt'
  	lines (1 <-> 7)
  	lines (3 <-> 5)
  [1]
  $ ls .promote/src
  ls: cannot access '.promote/src': No such file or directory
  [2]

When called with --check-only on valid files, exit 0
  $ ocomment --check-only ocomment-settings src/NoProblem.kt
