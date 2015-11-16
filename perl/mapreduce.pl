#!/usr/bin/perl

# please see README.md in this directory for notes.

use strict;
use warnings;
use 5.10.0;

my %counts;

die unless @ARGV == 1;

my $arg = shift;
if (-f $arg) {
    # given a single file name: do the "map" step for that file, producing an
    # intermediate output file
    map_out($arg);
} else {
    # otherwise this is the "main" call; $arg is number of parallel runs.
    map_reduce($arg);
}

exit 0;

# ---- map_reduce
sub map_reduce {
    my $nprocs = shift;

    # (main call) we're called with number of parallel "map" parts to run.
    # Start map jobs, wait for them to produce the intermediate files, then
    # "reduce"

    # start $nprocs parallel map jobs
    system("/bin/ls tmp/tweets/tweets_* | xargs -P $nprocs -I % $0 %");

    # parallel jobs done; now reduce
    @ARGV = glob("tmp/perl_temp_tweets_*");
    while (<>) {
        my ($nhood, $count) = split /\t/;
        $counts{$nhood} += $count;
    }
    unlink glob("tmp/perl_temp_tweets_*");

    open(my $out_fh, ">", "tmp/perl.output") or die;
    # this is where Elixir's pipe operator really looks cool :)
    map { print $out_fh "$_\t$counts{$_}\n" }
        sort { $counts{$b} <=> $counts{$a} or $a cmp $b }
            keys %counts;

}

# ----

sub map_out {
    # we're called with a specific file; this is one single "map" part
    my $in = shift;

    (my $out = $in) =~ s(/tweets/)(/perl_temp_);

    open(my $in_fh, "<", $in) or die;
    while (<$in_fh>) {
        next unless /knicks/i;
        my ($int, $nhood, $city, $message) = split /\t/;
        next unless $message =~ /knicks/i;
        $counts{$nhood}++;
    }

    open(my $out_fh, ">", $out) or die;
    map { print $out_fh "$_\t$counts{$_}\n" } keys %counts;
}
