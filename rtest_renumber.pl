#!/usr/bin/env perl

=head1 rtest_renumber.pl

  renumber the tests in an rtest_xxx.mac file

  usage:

  rtest_renumber.pl  /path/to/an/rtest.mac  /path/to/rtest2.mac ...

  Or, set the rtests to rewrite in the variable @Rtests below
  and put this variable in the for loop at the bottom of this file.

  To use, first prepare the original rtest file:
  Before each test in the file, insert something like this

  /* Test */
  
     or

  /* Test 1234 */

  There must be nothing else on these lines.
  There may any number of spaces between/around elements and any number of digits,
  or none. 
  The numbers don't have to be in any order, or some may be missing.
  You may disable one of the /* Test ... lines by putting any other characters
  on the line.

  This script rewrites the file, replacing each occurrence of /* Test ... above
  with the correct numbers inserted.

  The original file is backed up with a suffix `.back'

=cut

my @Rtests = qw (
 aex/rtests/rtest_aex.mac
 aex/rtests/rtest_afuncs.mac
 alt_eigen/rtests/rtest_alt_eigen.mac
 discrete_aex/rtests/rtest_discrete_aex.mac
 defmfun1/rtests/rtest_defmfun1.mac
 lists_aex/rtests/rtest_list.mac
 lists_aex/rtests/rtest_table.mac
 lists_aex/rtests/rtest_take.mac
 mext_defmfun1/rtests/rtest_mext_defmfun1.mac
 mext_system/rtests/rtest_load_in_subdir.mac
 numerical/rtests/rtest_mquad.mac
 numerical/rtests/rtest_numerical.mac
 pw/rtests/rtest_pw.mac
 test_defmfun1/rtests/rtest_test_defmfun1.mac
 tpsolve/rtests/rtest_to_poly.mac
  tpsolve/rtests/rtest_to_poly_solve.mac
 store/rtests/rtest_store.mac
);

my @files = @ARGV;

sub dosys { 
    my $c = shift; 
    print $c, "\n";
    system $c;
}

sub rewrite_one_rtest {
    my ($rtest) = @_;
    print "Rewriting $rtest\n";
    if ( not -e $rtest ) {
        die "File `$rtest' does not exist";
    }
    my $count = 0;
    open my $IH, '<', $rtest or die
        "Can't open `$rtest' for reading";
    my $backup = $rtest . '.back';
    dosys("cp -a $rtest $backup");
    my $outstr = '';
    while (<$IH>) {
        if ( /\s*\/\*\s*Test\s*\d*\s*\*\/\s*/ ) {
            $count ++;
            $outstr .= "/* Test $count */\n";
        }
        else {
            $outstr .= $_;
        }
    }
    close($IH);
    open my $OH , '>', $rtest or die
        "Can't open `$rtest' for writing";
    print $OH $outstr;
    close($OH);
}

foreach my $rtest (@Rtests) {
    rewrite_one_rtest($rtest);
}
