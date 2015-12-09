The Elixir implementation provides a mapreduce mix task that receives either "binary" or "regex" as argument.
The argument chooses which module will do the mapping over each files. Once mapping is performed with each file in a different Elixir process,
the result of the mapper is returned to a single reducer process which sums up all counts.

#### Binary

The "binary" argument chooses a BinaryMapActor which uses [binary matches provided by the Erlang VM to lookup for matches](http://www.erlang.org/doc/efficiency_guide/binaryhandling.html).
Because binary matches are case sensitive (while the solution is to be case insensitive),
the first step of the mapper algorithm is to generate all permutations for the word being counted.

#### Regex

The "regex" argument chooses a RegexMapActor which uses regular expressions and are often slower than binary matches.


#### Conclusion

Generally speaking, both solutions could be further optimized although we believe the current code provides a good trade-off between clarity and performing more low-level optimizations. Other than that, we expect future Elixir versions to yield even better results. For example, Elixir 1.2 introduces support to large maps which will perform faster than HashDict.

Do note that there is a lot of overlap between the Elixir and Erlang implementations due to Elixir being built on Erlang.
Further discussion can be found in this [pull request.](https://github.com/dimroc/etl-language-comparison/pull/10)
