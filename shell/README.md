This solution uses [GNU Parallel](http://www.gnu.org/software/parallel/). Time on my machine (Cygwin): 1.0s (compare results in [C# README](../csharp/README.md)).
The bottleneck is grep, so the particular implementation of grep can greatly influence the result. In particular, the grep that comes with MacOS seems to [perform poorly](http://jlebar.com/2012/11/28/GNU_grep_is_10x_faster_than_Mac_grep.html) in comparison to GNU grep.

Here are results for a few different implementations (all with GNU Parallel if not indicated otherwise):

Tool|Time in s
-------------|-----------
GNU grep -FUi|1.0
[ag](https://github.com/ggreer/the_silver_searcher) -Fi|1.1
ag w/o GNU Parallel|5.3
[FindStr](https://technet.microsoft.com/en-us/library/bb490907.aspx) /l /i|1.2
