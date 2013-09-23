#!/usr/bin/env perl

use strict;
use warnings;

# Make mersenne number data from file
# mersenne_number_data_from_wikipedia
# 

my (@ranks, @ps, @ndigs, @dates, @discoverers, @methods);

sub strips {
    my $s = shift;
    $s =~ s/^\s*//;
    $s =~ s/\s*$//;
    $s;
}

# Fields in copied data are split on tabs, not whitespace
while (<>) {
    next if /^\s*#/;
    s/\[[^\]]+\]//g;  # remove footnote marks
    s/\&/and/g; # replace ampersand with and
    s/\,//g; # remove commas. Only used in numbers
    
#    my @fields = split(/\t/,$_);
#    print scalar(@fields), "\n";

    my($rank, $p, $approx, $ndig, $date, $discoverer, $method) = split(/\t/,$_);
    push @ps, $p;
    push @ranks, $rank;
    push @dates, strips($date);
    push @ndigs, strips($ndig);
    push @discoverers, strips($discoverer);
    push @methods, strips($method);
}

print "(defvar *mersenne-prime-num-digits* '(\n ",
    join("\n ",@ndigs), "))\n\n";

print "(defvar *mersenne-numbers-full-date* '(\n ";
print ' "' . $_ . '" ' . "\n" foreach (@dates);
print "))\n\n";

print "(defvar *mersenne-numbers-full-discoverer* '(\n ";
print ' "' . $_ . '" ' . "\n" foreach (@discoverers);
print "))\n\n";

print "(defvar *mersenne-numbers-method* '(\n ";
print ' "' . $_ . '" ' . "\n" foreach (@methods);
print "))\n\n";
