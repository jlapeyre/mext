## Scripts for building mext and packages and running tests

These instructions assume the current directory
is the **top level of the mext distribution**.

These instructions assume the current directory
is the **top level of the mext distribution** !!

These instructions assume the current directory
is the **top level of the mext distribution** !!!!

### Test mext with several maximas in parallel

```sh
 ./bin/text_mext.sh
```

This script builds and tests the mext system and several packages
simultaneously for several specified versions of maxima. The maxima
binaries and test directories are labeled by maxima version and lisp
implementation and version.

See comments at the top of `./bin/text_mext.sh`

### Build each package with a separate maxima process

```sh
 ./bin/text_mext2.sh
```

This is the same as `./bin/text_mext.sh` except each
a new maxima process is started for each package.
Building lapack may fail otherwise.

`bin/text_mext2.sh` depends on `bin/buildall1.mac`, `bin/testall1.mac`,
`install_mext.sh`, `bin/parse_testlog.pl`

### Testing procedure

(Almost) All files used in testing are in the directory `../mext_test_top`. That is,
above the `mext` top level, outside the mext distribution. A test subdirectory
is created for each maxima version. For instance `../mext_test_top/maxima-5.41.0-sbcl-1.4.6`.
The mext source distribution is copied to this test subdirectory. 

The exception to all files for testing being in `../mext_test_top` are the built mext
packages, which are copied to the user's `.maxima` directory to subdirectories
name for the maxima and lisp versions.

A build log `maxima-5.41.0-sbcl-1.4.6.mextlog` and a test log  `maxima-5.41.0-sbcl-1.4.6.testlog`
are written to test subdirectory. A perl script is run at the end to pull and collate information
from the test log.

### Renumber rtests

The tests in rtest files should be numbered in order. When adding or
moving tests, the numbers are no longer sequential.

To renumber all rtests that are listed inside the renumbering
script. The script may be invoked as follows

```sh
bin/rtest_renumber.pl -d packages        # renumber packages
bin/rtest_renumber.pl --dry -d packages  # dry run
bin/rtest_renumber.pl --help             # help
```

### Create a new mext package

To make a directory and template for a mext package

```sh
./bin/make_mext.pl newpackname
```

### Other test scripts

The following is obsolete

```sh
./bin/all-lisps.sh
```

### Build documentation in pdf format

```sh
./bin/builddoc.sh
```
