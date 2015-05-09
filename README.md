# Update

Please see the following blog posts for the latests updates:

1. [ETL Language Showdown](http://www.dimroc.com/2014/09/29/etl-language-showdown/) - Sept. 2014
2. [ETL Language Showdown Part 2 - Now with Python](http://www.dimroc.com/2015/02/26/etl-language-showdown-pt2/) - Feb. 2015

## ETL Language Showdown
This repo implements the same map reduce ETL (Extract-Transform-Load) task in multiple languages
in an effort to compare language productivity, terseness and readability. The performance comparisons should not be taken seriously. If anything,
it is a bigger indication of my skillset in that language rather than their performance capabilities.

## The Task
Count the number of tweets that mention 'knicks' in their message and bucket based on the neighborhood of origin.
The ~1GB dataset for this task, sampled below, contains a tweet's message and its NYC neighborhood. [It can be downloaded here](https://dimroc-public.s3.amazonaws.com/etl-language-comparison/tweets20140416.tar.gz).

```
91	west-brighton	Brooklyn	Uhhh
121	turtle-bay-east-midtown	Manhattan	Say anything
175	morningside-heights	Manhattan	It feels half-cheating half-fulfilling to cite myself.
```

### Initial Assumption

* These tasks are not run on Hadoop but do run concurrently. Performance numbers are moot since the CPU mostly sits idle waiting on Disk IO.
* **UPDATE: Boy was the IO bound assumption wrong.

## The Languages

1. [Ruby 2.2.2](https://www.ruby-lang.org/en/news/2015/04/13/ruby-2-2-2-released/)
3. [Golang 1.4.2](http://golang.org/) - Imperative
4. [Scala 2.11.4](http://scala-lang.org/) - Both Imperative and Functional
5. [Elixir 1.0.4](http://elixir-lang.org/) - Functional
6. [Python 3](https://www.python.org/)

### Ruby

- Celluloid Actor Pool
- Span multiple processes with [grosser/parallel](https://github.com/grosser/parallel)

### Scala

- Akka (Supervisors and Actors)

## Results

<table>
  <tr>
    <td>Ruby w/ Celluloid (Global Interpreter Lock Bound, single core)</td>
    <td>43.745s</td>
  </tr>

  <tr>
    <td>JRuby w/ Celluloid</td>
    <td>15.855s</td>
  </tr>

  <tr>
    <td>Ruby w/ <a href="https://github.com/grosser/parallel" target="_blank">grosser/parallel</a> (<b>not</b> GNU Parallel)</td>
    <td>12.449s</td>
  </tr>

  <tr>
    <td>Python w/ <a href="https://docs.python.org/2/library/multiprocessing.html" target="_blank">Pool</a></td>
    <td>12.730s</td>
  </tr>

  <tr>
    <td>Scala</td>
    <td>8.8s</td>
  </tr>

  <tr>
    <td>Golang</td>
    <td>TODO</td>
  </tr>

  <tr>
    <td>Elixir</td>
    <td>21.84s</td>
  </tr>

  <tr>
    <td>Node w/ [Cluster](https://nodejs.org/docs/latest/api/cluster.html)</td>
    <td>TODO</td>
  </tr>
</table>

