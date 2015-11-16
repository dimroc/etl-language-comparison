#!/usr/bin/perl

# please see README.md in this directory for notes.

use strict;
use warnings;
use 5.10.0;

my %counts;

@ARGV = glob("tmp/tweets/tweets_*");

while (<>) {
    next unless /knicks/i;
    my ($int, $nhood, $city, $message) = split /\t/;
    next unless $message =~ /knicks/i;
    $counts{$nhood}++;
}

open(my $out_fh, ">", "tmp/perl-no-mapreduce.output") or die;
# this is where Elixir's pipe operator really looks cool :)
map { print $out_fh "$_\t$counts{$_}\n" }
    sort { $counts{$b} <=> $counts{$a} or $a cmp $b }
        keys %counts;
