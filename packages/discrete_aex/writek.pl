#!/usr/bin/env perl

use v5.14.0;

print "(defvar *khinchine-as-integer*\n";

my $count = 0;

while(<>){
    next if /#/;
    s/\://g;
    my (@nums) = split;
    foreach my $n (@nums) {
        $count++;
#        say length($n);
        print $n if length($n) > 9;
    }
}

print "\n";
print ")\n";

print STDERR "$count numbers\n";
