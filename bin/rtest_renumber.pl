#!/usr/bin/env perl
use strict;
use warnings;
use File::Copy;
use Getopt::Long;
use Pod::Usage;
use File::Spec::Functions;

=head1 NAME 

I<rtest_renumber.pl> - renumber the tests in an rtest_xxx.mac file.

=head1 SYNOPSIS

Renumber specified files

  rtest_renumber.pl  /path/to/an/rtest.mac  /path/to/rtest2.mac ...
 
Renumber tests in files specified in the source I<rtest_renumber.pl>

  rtest_renumber.pl

=head1 DESCRIPTION

Renumbers the tests in rtest files for the Maxima CAS. The files to
renumber may be given on the command line. If no files are given, then
the files given in the variable C<@Rtests> in the script itself are
renumbered.

Before renumbering an rtest file, it must be prepared by inserting a comment before
each test. The simplest way to do this is to insert this line before each test

/* Test */

A test number will be inserted in any line of this form. All other lines will
be copied unaltered. Later, when tests are inserted/removed/moved, running
I<rtest_renumber.pl> will rewrite the test numbers.

This script rewrites the file, replacing each occurrence of 
C</* Test */> with the correct numbers inserted.  The original file is
backed up with a suffix `.back'

More generally, exactly the lines that are renumbered are those that
consist of: zero or more spaces, the string "/*", zero or more spaces,
the string "Test", zero or more spaces, a sequence of zero or more
digits, zero or more spaces, the string "*/", zero or more
spaces, newline. All other lines are reproduced unaltered.

For example, any of the following lines will be renumbered

 /* Test */
  /* Test    */
 /* Test 1234 */

For example, lines such as the following will be left untouched and will have no
effect on numbering the tests.

 /* Test of the new algorithm */
 /* Test x     */
 /* Test 11 disabled */
 /* Test 1 2 */

=head1 OPTIONS

=over 4

=item B<-d> | B<--dir> 

Prepend the directory B<dir> to the pathname of each rtest.
For example, the rtest files listed in this script
are given like

 packname/rtests/rtest_packname.mac

So, to renumber all tests from the top-level of mext, type

 ./bin/rtest_renumber.pl -d packages

=item B<--dry>

Dry run. No files are written, copied, or changed. But all
messages are printed as if they were changed.

=item B<-q> | B<--quiet> 

Do not print messages while running.

=item B<-v> | B<--verbose> 

Print more messages while running.

=item B<--man>

Print the manual page.

=item B<--help> B<-h>

Print shorter help.

=back

=head1 AUTHOR

John Lapeyre

=cut

# To find some files that are not prepared for numbering:
# grep -L  '/\* Test'  packages/*/rtests/*.mac

my @Rtests = qw (
 aex/rtests/rtest_aex.mac
 aex/rtests/rtest_afuncs.mac
 aex/rtests/rtest_type_of.mac
 alt_eigen/rtests/rtest_alt_eigen.mac
 discrete_aex/rtests/rtest_discrete_aex.mac
 defmfun1/rtests/rtest_defmfun1.mac
 fourier_elim_mext/rtests/rtest_fourier_elim.mac
 implicit/rtests/rtest_implicit.mac
 lists_aex/rtests/rtest_list.mac
 lists_aex/rtests/rtest_table.mac
 lists_aex/rtests/rtest_take.mac
 maxdoc/rtests/rtest_maxdoc.mac
 mext_basic/rtests/rtest_mext_basic.mac
 mext_basic/rtests/rtest_misc.mac
 mext_system/rtests/rtest_load_in_subdir.mac
 numerical/rtests/rtest_mquad.mac
 numerical/rtests/rtest_numerical.mac
 pw/rtests/rtest_pw.mac
 test_defmfun1/rtests/rtest_test_defmfun1.mac
 to_poly_mext/rtests/rtest_to_poly.mac
 tpsolve/rtests/rtest_to_poly_solve.mac
 runtime/rtests/rtest_runtime.mac
 store/rtests/rtest_store.mac
);

my $Quiet = 0;
my $Verbose = 0;
my $Man = 0;
my $Help = 0;
my $Totalcount = 0;
my $Toplevel = undef;
my $Dry = 0;

GetOptions ("quiet|q" => \$Quiet,
            "verbose|v" => \$Verbose,
            "help|h" => \$Help,
            "man" => \$Man,
            "dir|d=s" => \$Toplevel,
            "dry" => \$Dry)
 or die("*** rtest_renumber: Error in command line arguments: $!");

pod2usage( -verbose => 2 ) if $Man;
pod2usage( -verbose => 1 ) if $Help;


$Quiet = 0 if $Verbose;

my @Files = @ARGV;

#sub dosys { 
#    my $c = shift; 
#    print $c, "\n";
#    system $c;
#}

sub rewrite_one_rtest {
    my ($rtest) = @_;
    if ($Toplevel) {
        $rtest = catfile($Toplevel,$rtest);
    }
    if ( not -e $rtest ) {
        die "*** File `$rtest' does not exist.";
    }
    my $count = 0;
    open my $IH, '<', $rtest or die
        "*** Can't open `$rtest' for reading: $!";
    my $outstr = '';
    my $origstr = '';
    my $renumbered_flag = 0;
    print "Reading `$rtest'.\n" if $Verbose;
    while (<$IH>) {
        $origstr .= $_;
        if ( /\s*\/\*\s*Test\s*(\d*)\s*\*\/\s*/ ) {
            my $old_number = $1;
            $count ++;
            $renumbered_flag = 1 unless $old_number and $old_number == $count;
            $outstr .= "/* Test $count */\n";
        }
        else {
            $outstr .= $_;
        }
    }
    close($IH);
    # this is redundant, both checks work.
    if ( ($origstr eq $outstr) or (not $renumbered_flag)) {
        print "Not rewriting `$rtest'; no change.\n" unless $Quiet;
        return;
    }
    else {
        print "Rewriting `$rtest'.\n" unless $Quiet;
        $Totalcount += $count;
        my $backup = $rtest . '.back';
        print "Copying `$rtest' to `$backup'.\n" if $Verbose;
        if (not $Dry) {
            copy($rtest,$backup) or die "*** Copy `$rtest' to `$backup' failed: $!";
        }
        print "Writing `$rtest'.\n" if $Verbose;
        if (not $Dry) {
            open my $OH , '>', $rtest or die
                "*** Can't open `$rtest' for writing";
            print $OH $outstr or die "*** Writing line to file `$rtest' failed: $!";
            close($OH);
        }
    }
}

my $files_to_use;
if (@Files) {
    $files_to_use = \@Files;
}
else {
    $files_to_use = \@Rtests;
}

foreach my $rtest (@$files_to_use) {
    rewrite_one_rtest($rtest);
}
print "$Totalcount tests renumbered.\n" unless $Quiet;
