When called with no filepath
Then Exit 0
And no promote file generated
  $ ocomment ocomment-settings
  $ ls .promote
  ls: cannot access '.promote': No such file or directory
  [2]

Given a src folder with two invalid files and one valid file
  $ ls src
  Example.java
  Example.kt
  NoProblem.kt

When calling with src/*
Then non-zero exit code
And the promotion folder is created with two promote files
And the promote files are the invalid files in src with corrected hashes
  $ ocomment ocomment-settings src/*
  [1]
  $ ls .promote/src
  Example.java.promote
  Example.kt.promote
  $ cat .promote/src/Example.java.promote
  public class Main {
      public static void main(String[] args) {
          // <# Slight variation on an old classic
          System.out.println("HellO-caml!")
          // #> 1011f1c5c41f1007092bfbad3e9003ef
      }
  }
  $ cat .promote/src/Example.kt.promote
  // <# Definitely worth checksuming
  fun main() {
      println("HellO-comment")
  }
  // #> 8c679bc9e375ffcb9294e8b858f5f46c

Tear down
  $ rm -rf .promote

When called with only one of the files
Then the promotion folder is created with only one promote file
And the promote file is the file with corrected hash
  $ ocomment ocomment-settings src/Example.java
  [1]
  $ ls .promote/src
  Example.java.promote
  $ cat .promote/src/Example.java.promote
  public class Main {
      public static void main(String[] args) {
          // <# Slight variation on an old classic
          System.out.println("HellO-caml!")
          // #> 1011f1c5c41f1007092bfbad3e9003ef
      }
  }
