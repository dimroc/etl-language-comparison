# notes plus comments

## the perl implementation

1.  I'm using xargs to drive the parallelism and intermediate files to
    communicate between the map and reduce steps.  I know perl has at least
    one, maybe more, parallelising libraries in CPAN, but I want to see what
    core perl can do on this dataset.  Plus I'm lazy...

2.  The intermediate files go into "tmp" with predictable names; we're not
    doing the "safe tmpdir" dance.

## results

I tested only Ruby, Erlang, Elixir, and Python.  Rough results on my laptop
(HP something, 4 GB, i7, 2x2 CPUs, Fedora 22, 32-bit), are:

                            cold cache (*)  warm cache

    new code:
        shell 1-liner       13.2             2.6        (see below)
        perl/no-mapreduce   12.3             6.3
        perl/mapreduce      16.7             3.2

    existing code:
        elixir binary       15.8             5.9
        elixir regex        32.1            13.4
        erlang binary       26.8             9.8
        erlang regex        36.3            11.6
        python              44.4            22.5
        ruby                45.5            38.8
        ruby-parallel       40.0            20.1

Cold cache means you first run `echo 3 > /proc/sys/vm/drop_caches` as root.

The shell 1-liner is:

    grep -h -i knicks tmp/tweets/tweets_* | cut -f2 | sort | uniq -c | sort -n -r > tmp/shell.output

This searches the whole line, including the "city" and "neighbourhood" fields
and not just the "message" field, for the pattern.  That's arguably a bug, but
it's still worth comparing, if only to get a feel for how fast it might be.
(And doesn't take much effort to write!)

# side notes

## (about me)

Long time perl guy, love the language, believe xkcd 224 is actual fact.
Dabbling in Elixir because of some really neat features.  **Not** too worried
about raw speed but this was a simple and fun exercise so I jumped in.

## perl speed, "IO bound"-ness

The serial version (no-mapreduce) blows almost everything else away.  Some
lessons there.  And it certainly looks IO-bound to me.  Consider:

*   with a cold cache, the serial version in perl, and the shell 1-liner, are
    the fastest
*   with a warm cache, the data set fits entirely in buffer/cache on my
    laptop, which means "IO" is basically "CPU+RAM".  Then the map-reduce
    version becomes faster than the serial.

## python speed, Unicode

I'm actually surprised; I had always heard that python is plenty fast, and I
can't help wondering if I screwed up somewhere.  Or maybe it's better in
64-bit (my laptop is 32-bit).

Also, this is the only version that seems to handle Unicode properly.  (Even
my perl version doesn't; patches welcome!).  The `tweets_al` file has the
following word in the message field somewhere:

    120	midtown-midtown-south	Manhattan	#NYK#KNİCKS [...]

Notice the `İ` there?  Only the python version catches it and counts it!

## Elixir/Erlang speed, fitness, low-level code

First, this benchmark doesn't really play to Erlang/Elixir's strengths.  None
of the features that got me interested in Elixir can truly be brought to play
(AFAICS).

That said, the "binary actor" stuff seems disappointingly low-level.  The code
in `Util.parse_hood` is not too different from what you might write in C using
`strstr(3)`.  The `permute_case` part is another downer.  If Jose ever runs
out of ideas on "what next to improve", this may be a good candidate for his
special brand of magic :)

Actually, both these versions have a subtle bug: they'll count tweets where
the desired pattern appears in the "neighbourhod" or "city" fields also.  I
know there aren't any New York neighbourhoods or cities with names containing
"knicks", but I mention this because I am pretty sure it will non-trivially
impact the binary actor code.

## Ruby speed (or lack thereof!)

Ruby's parallel version on a warm cache is slower than perl's serial version
on a cold cache.  Even worse, it's serial version appears to gain almost
nothing from a warm cache.  Again, it is possible I screwed up somehow, but
that's what it looks like at the moment.

