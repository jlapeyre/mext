## Scripts for building mext and packages and running tests

These instructions assume the current directory
is the top level of the mext distribution.

### Test mext with several maximas in parallel

```sh
 ./bin/text_mext.sh
```

See comments at the top of text_mext.sh

### Build each package with a separate maxima process

```sh
 ./bin/text_mext2.sh
```

This is the same as `./bin/text_mext.sh` except each
a new maxima process is started for each package.
Building lapack may fail otherwise.

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
