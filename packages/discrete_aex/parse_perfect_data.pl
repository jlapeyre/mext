#!/usr/bin/env perl

use strict;
use warnings;

# Make perfect number data from file
# perfect_number_data_from_wikipedia
# ./parse_perfect_data.pl < mersenne_number_data_from_wikipedia > mersenne-data.lisp

my (@ranks, @ps, @ndigs, @years, @notes);

while (<>) {
    next if /^\s*#/;
    my($rank, $p, $approx, $ndig, $year, @rest) = split;
    push @ps, $p;
    push @ranks, $rank;
    push @years, $year;
    push @ndigs, $ndig;
    push @notes, join(" ",@rest);
}

print "(defvar *mersenne-exponents* '(\n ",
    join("\n ",@ps), "))\n\n";

print "(defvar *perfect-numbers-num-digits* '(\n ",
    join("\n ",@ndigs), "))\n\n";

# Don't quote to make strings
print "(defvar *mersenne-numbers-year* '(\n ";
print ' ' . $_ . ' ' . "\n" foreach (@years);
print "))\n\n";


print "(defvar *mersenne-numbers-discoverer* '(\n ";
print ' "' . $_ . '"' . "\n" foreach (@notes);
print "))\n\n";
