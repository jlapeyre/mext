#!/usr/bin/env perl
use strict;
use warnings;

=head1 NAME

parse_testlog.pl

=head1 USAGE

  parse_testlog.pl type file1 file2 ...

=head1 DESCRIPTION

  Print some of the output from either building or testing maxima mext.
  If type is 'build', then the log file is expected to be from building. If
  type is 'test, then the log file is expected to be from rtests.

=cut

my $Logfile;

sub lfline {
    my ($line) = @_;
    $Logfile . ': ' . $line;
}

sub prepend {
    my ($line) = @_;
    lfline($line);
}

sub sprepend {
    my ($line) = @_;
    '  ' . lfline($line);
}

sub seprepend {
    my ($line) = @_;
    '  *** ' . lfline($line);
}

sub eprepend {
    my (@lines) = @_;
    my $res = '';
    foreach my $line (@lines) {
        chomp($line);
        $res .= seprepend($line) . "\n" if $line;
    }
    $res;
}

# read log file and return ref to list of lines.
sub read_log_file {
    my ($fname) = @_;
    open my $fh, '<', $fname or die "Can't open log file '$fname' for reading";
    my @lines = <$fh>;
    close($fh);
    \@lines;
}

sub parse_test_log_file {
    my ($lines) = @_;
    my @tests = ( 
        [ qr/^Running tests in/ , sub { $_[1]->{running} = $_[0] } ],
        [ qr/No unexpected errors/ , sub { print prepend(shift);}],
        [ qr/The following /, sub {print seprepend($_[1]->{running}); print seprepend(shift);}],
        [ qr/failed out of/, sub {print eprepend(shift)}],
        [ qr/an error/, sub {print eprepend(shift)}],
        [ qr/Unable to find/, sub {print eprepend(shift)}],
        [ qr/Maxima encountered a Lisp error/,
          sub { 
              my $h = $_[1];
              my $i1 = $h->{line_count};
              my $i2 = $h->{line_count}+3;
              print eprepend(@{$h->{lines}}[$i1 .. $i2]);
          }]
        );
    parse_file($lines,\@tests);
}

sub parse_build_log_file {
    my ($lines) = @_;
    my @tests = ( 
        [ qr/Maxima encountered a Lisp error/,
          sub { 
              my $h = $_[1];
              my $i1 = $h->{line_count};
              my $i2 = $h->{line_count} + 3;
              print eprepend(@{$h->{lines}}[$i1 .. $i2]);
          }]
        );
    parse_file($lines,\@tests);
}


sub parse_file {
    my ($lines,$tests) = @_;
    my $running;
    my $line_count = -1;
    my $line;
    my $data = {lines => $lines};
    foreach my $line (@$lines) {
        $line_count++;
        $data->{line_count} = $line_count;
        foreach my $test (@$tests) {
            my $regex = $test->[0];
            my $code = $test->[1];
            &$code($line,$data) if ($line =~ /$regex/);
        }
    }
}

my $type = shift(@ARGV);

foreach my $logfile (@ARGV) {
    $Logfile = $logfile;
    my $lines = read_log_file($logfile);
    parse_test_log_file($lines) if $type eq 'test';
    parse_build_log_file($lines) if $type eq 'build';
    print "\n";
}
