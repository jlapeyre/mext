#!/usr/bin/env perl

=head1 rtest_renumber.pl

  renumber the tests in an rtest_xxx.mac file

  usage:

  rtest_renumber.pl  < /path/to/rtest_xxx.mac > /path/to/new/rtest_xxx.mac

  To use, first prepare the original rtest file:
  Before each test in the file, insert something like this

  /* Test */
  
     or

  /* Test 1234 */

  There must be nothing else on these lines.
  There may any number of spaces between/around elements and any number of digits,
  or none. The numbers don't have to be in any order, or some may be missing.

  This script rewrites the file, replacing each occurrence of /* Test ... above
  with the correct numbers inserted.

=cut

my $count = 0;
while (<>) {
    if ( /\s*\/\*\s*Test\s*\d*\s*\*\/\s*/ ) {
        $count ++;
        print "/* Test $count */\n";
    }
    else {
        print;
    }
}
