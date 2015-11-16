This solution uses [GNU Parallel](http://www.gnu.org/software/parallel/). Time on my machine (Cygwin): 1.0s (compare results in [C# README](../csharp/README.md)).
The bottleneck is grep, so the particular implementation of grep can greatly influence the result. In particular, the grep that comes with MacOS seems to [perform poorly](http://jlebar.com/2012/11/28/GNU_grep_is_10x_faster_than_Mac_grep.html) in comparison to GNU grep.

