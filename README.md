# Ocomment
File portion checksum enforcer aimed to reduce comments entropy

## Usage

```bash
./ocomment <ocomment_settings_file> filepath_1 filepath_2 ... filepath_n
```

Where **ocomment_settings_file** should follow this structure:

```clojure
((promotion_dir test/resources/.promote)
   (includes (
    (.java ((start_marker "// <#") (end_marker "// #>")))
    (.py ((start_marker "# ocomment start") (end_marker "# ocomment end")))
    (...)
    (.<file_extension> ((start_marker "start?") (end_marker "end?"))))))
```

The **promotion_dir** field indicate the folder where to generate corrected files when invalid checksum are detected.

The **includes** section is a list of association between extensions and markers. All files passed as argument to the executable matching one of these extensions will be validated using the corresponding markers.

### Examples :

#### Validate one file

```bash
./ocomment .ocomment src/main/com/app/Hello.java
```

#### Validate all .py files in a 'src' folder

```bash
./ocomment .ocomment src/**/*.py
```

NB : If no .py extension markers are configured in the .ocomment this would have no effect

#### Validate all files modified since last commit

```bash
./ocomment .ocomment $(git diff --name-only)
```

#### Validate all files modified between the last two commits in a given folder

```bash
./ocomment .ocomment $(git diff --name-only HEAD HEAD~ src/)
```
