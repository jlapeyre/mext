Assume we are at the top of the mext distribution

To test mext with several maximas in parallel

 ./bin/text_mext.sh

See comments at the top of text_mext.sh

This is the same, but builds each package with a separate maxima
process. Building lapack may fail otherwise.

 ./bin/text_mext2.sh

To renumber all rtests that are listed inside the renumbering
script

 bin/rtest_renumber.pl -d packages
 bin/rtest_renumber.pl --dry -d packages  # dry run
 bin/rtest_renumber.pl --help # help

To make a template for a mext package
 ./bin/make_mext.pl newpackname
