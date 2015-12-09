# Update

Please see the following blog posts for the latests updates:

1. [ETL Language Showdown](http://blog.dimroc.com/2014/09/29/etl-language-showdown/) - Sept. 2014
2. [ETL Language Showdown Part 2 - Now with Python](http://blog.dimroc.com/2015/05/07/etl-language-showdown-pt2/) - May. 2015
3. [ETL Language Showdown Part 3 - 10 Languages and growing](http://blog.dimroc.com/2015/11/14/etl-language-showdown-pt3/) - Nov. 2015

----

## Wins

Analyses and discussions done here have led to the following language pull requests:

1. [Add BIF binary:split/2,3 to Erlang](https://github.com/erlang/otp/pull/771)
2. [Improve case insensitive regex to Golang](https://github.com/golang/go/issues/13288)

## ETL Language Showdown
This repo implements the same map reduce ETL (Extract-Transform-Load) task in multiple languages
in an effort to compare language productivity, terseness and readability. The performance comparisons should not be taken seriously. If anything,
it is a bigger indication of my skillset in that language rather than their performance capabilities.

## The Task
Count the number of tweets that mention 'knicks' in their message and bucket based on the neighborhood of origin.
The ~1GB dataset for this task, sampled below, contains a tweet's message and its NYC neighborhood.

Simply run `fetch_tweets` in the repo directory or [downloaded here](https://dimroc-public.s3.amazonaws.com/etl-language-comparison/tweets20140416.tar.gz).

```
91	west-brighton	Brooklyn	Uhhh
121	turtle-bay-east-midtown	Manhattan	Say anything
175	morningside-heights	Manhattan	It feels half-cheating half-fulfilling to cite myself.
```

### Initial Assumption

* These tasks are not run on Hadoop but do run concurrently. Performance numbers are moot since the CPU mostly sits idle waiting on Disk IO.
* **UPDATE: Boy was the IO bound assumption wrong.

## The Languages

Below you will find the languages run. Note that frameworks also play a big role, for example the Scala implementation
compares the parallel collection to futures and the Akka framework. Click through on each language to read more.


<table>
<tr> <th>Language</th><th>Owner</th> </tr>
<tr> <td><a href="https://github.com/dimroc/etl-language-comparison/tree/master/ruby">Ruby</a></td><td> </td> </tr>
<tr> <td><a href="https://github.com/dimroc/etl-language-comparison/tree/master/golang">Golang</a></td><td></td> </tr>
<tr> <td><a href="https://github.com/dimroc/etl-language-comparison/tree/master/scala">Scala</a></td><td></td> </tr>
<tr> <td><a href="https://github.com/dimroc/etl-language-comparison/tree/master/nim">Nim</a></td><td></td> </tr>
<tr> <td><a href="https://github.com/dimroc/etl-language-comparison/tree/master/nodejs">Node</a></td><td></td> </tr>
<tr> <td><a href="https://github.com/dimroc/etl-language-comparison/tree/master/php">PHP</a></td><td></td> </tr>
<tr> <td><a href="https://github.com/dimroc/etl-language-comparison/tree/master/erlang">Erlang</a></td><td></td> </tr>
<tr> <td><a href="https://github.com/dimroc/etl-language-comparison/tree/master/elixir">Elixir</a></td><td><a href="https://github.com/josevalim">josevalim</a></td> </tr>
<tr> <td><a href="https://github.com/dimroc/etl-language-comparison/tree/master/rust">Rust</a></td><td></td> </tr>
<tr> <td><a href="https://github.com/dimroc/etl-language-comparison/tree/master/python">Python</a></td><td></td> </tr>
<tr> <td><a href="https://github.com/dimroc/etl-language-comparison/tree/master/csharp">C#</a></td><td><a href="https://github.com/mganss">mganss</a></td> </tr>
<tr> <td><a href="https://github.com/dimroc/etl-language-comparison/tree/master/shell">shell</a></td><td><a href="https://github.com/mganss">mganss</a></td> </tr>
<tr> <td><a href="https://github.com/dimroc/etl-language-comparison/tree/master/perl">perl</a></td><td><a href="https://github.com/sitaramc">sitaramc</a></td> </tr>
</table>

