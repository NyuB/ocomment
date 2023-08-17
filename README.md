# Ocomment
File portion checksum enforcer aimed to reduce comments entropy

## Get the binary
How to obtain the **ocomment** binary

### Build directly from sources (requires opam)

```bash
opam switch create ./ --deps-only --with-test
eval $(opam env)
dune build
cp -L _build/install/default/bin/ocomment ./ocomment
```

### Use the CI docker image (requires docker)

```bash
docker build -t ocm:builder -f ci/Dockerfile.ci .
docker run --name ocm-builder -e DUNE_CMD=build ocm:builder
docker cp -L ocm-builder:/home/opam/project/_build/install/default/bin/ocomment ./ocomment
docker rm ocm-builder
```

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

#### Usage examples in tests

See the [test-bin](test-bin) folder (using [cram tests](https://bitheap.org/cram/)) for some usage examples

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
