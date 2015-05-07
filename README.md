# Update

Please see the following blog posts for the latests updates:

1. [ETL Language Showdown](http://www.dimroc.com/2014/09/29/etl-language-showdown/) - Sept. 2014
2. [ETL Language Showdown Part 2 - Now with Python](http://www.dimroc.com/2015/02/26/2015-02-26-etl-language-showdown-pt2/) - Feb. 2015

The rest of this README details Sept. 2014's post and has not been updated for Part 2.

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

1. [Ruby 2.1.0](https://www.ruby-lang.org/en/news/2013/12/25/ruby-2-1-0-is-released/) with [Celluloid](http://celluloid.io/)  - Exposes the GIL limitation in pure ruby and shows the multicore advantage of JRuby.
2. [Ruby 2.1.0](https://www.ruby-lang.org/en/news/2013/12/25/ruby-2-1-0-is-released/) and [GNU Parallel](http://www.gnu.org/software/parallel/)  - Uses GNU parallel to run ruby processes on multiple cores.
3. [Golang 1.2](http://golang.org/) - Imperative
4. [Scala 2.10.4](http://scala-lang.org/) - Both Imperative and Functional
5. [Elixir 1.0](http://elixir-lang.org/) - Functional
6. [Python 3](https://www.python.org/)

### Ruby

```
$ ./run_ruby
```

#### Features used

- Celluloid Actor Pool

#### Observations

- Performance is very respectable when considering the GIL lock: `1m15.243s`
- Performance is great when run on **JRuby**, which uses all available cores: `0m41.268s`

### Ruby with GNU Parallel

```
$ ./run_ruby_parallel
```

This is effectively:

```
$ parallel -j 90% -a commands.txt && ruby reducer.rb
```

#### Features used

- GNU Parallel to get around the GIL and more accurately mirror a real world scenario: Many single core workers running one ruby process (eg: Heroku dynos)

#### Observations

- Performance is excellent, with all cores on full blast: `40s`.
- This implementation is cheating in some areas but serves as a good baseline for other comparisons.
- Separate processes can be a maintenance nightmare. It leads to memory bloat, is difficult to coordinate failed processes, and can be difficult to deploy and scale. There is simplicity in being able to deploy one process that is capable of using all cores.
- From experience, Ruby's real weakness is its poor performance handling long-running jobs. Memory leaks run rampant. [Twitter shared this opinion](http://blog.redfin.com/devblog/2010/05/how_and_why_twitter_uses_scala.html#.U10CzWRdXLh).

### Golang

```
$ ./run_go
```

#### Features used

- goroutines
- channels
- selects

#### Observations

- Performance after first write with no optimizations: `3m23.165s`. **Was only using one core!**
- Performance average after using all cores by manually setting GOMAXPROCS: `1m03.593s`
- [Had to research why all cores weren't used here](http://stackoverflow.com/questions/17868419/how-can-my-go-program-keep-all-the-cpu-cores-busy).
- Ultimately, [GOMAXPROCS will be removed](http://golang.org/pkg/runtime/#GOMAXPROCS) and scheduling will automatically make use of all cores.
- Golang's libraries are fantastic but don't have the mature optimizations of other languages (yet).
- Ended up being the fewest lines of code across all languages, by a lot.
- Golang is not functional, so don't force functional programming concepts, like map and reduce. For loops for days...

#### Moments of Joy
- Handling goroutines with `channel`s and `select`.

    ```go
    for _ = range inputFiles {
      select {
      case <-channel:
        fmt.Println("Finished mapping.")
      }
    }
    ```

- Iterating over a `map` with `range`.
- Using `defer` for cleanup of file resources.
- Command-line debugger (but I didn't need it).

#### Moments of Disappointment
- Verbose error handling. There are design patterns to better manage errors, but were skipped for this demo.

    ```go
    files, err := ioutil.ReadDir(inputDir)
    if err != nil {
      panic(err)
    }
    ```

- Having to explicitly set the number of cores to use via `GOMAXPROCS` because of immature scheduling.
- Lack of collection helpers like `map` and `reduce`.

### Scala

```
$ ./run_scala
```

#### Features used

- Akka (Supervisors and Actors)

#### Observations

- Performance after first write on first run: `50s`
- Performance on subsequent runs: `27s`. The JVM is probably doing something fancy.
- All cores used.
- Not as IO bound as originally thought. Attributed to the optimizations in the BufferedSource/BufferedWriter classes.

#### Moments of Joy
- Witnessing the speed after the first write.
- Seeing BDD style testing as default for ScalaTest.
- Using `!`, `?`, and `receive` to handle messages in the Actor system.

    ```scala
    def map(inputDir: String, outputDir: String) = {
      val system = ActorSystem("MapSystem")
      val mapSupervisor = system.actorOf(Props[MapSupervisor], "mapsupervisor")
      val future = mapSupervisor ? ProcessDirectoryMessage(inputDir, outputDir)

      Await.result(future, Duration.Inf)
      system.shutdown
    }
    ```

- `sbt run` and `sbt test` work well, especially for fetching dependencies.
- Realizing the power of Akka and Akka Cluster.

#### Moments of Disappointment
- Inability to debug via the command line.
- Having to set implicit variables: `implicit val timeout = Timeout(5 minutes)`.
- Having to use Java libraries for File IO.

### Elixir

```
$ ./run_elixir
```

#### Features used

- Streams
- pipeline operators
- PIDs
- All the Erlang and Elixir goodness

#### Observations (Updated to reflect changes in Elixir 1.0)
- Performance average after first write with `:delayed_write`: `55.964s`.
- This number says less about Elixir's performance and more about how much I suck at writing Elixir code. Ease of writing performant code though is a valid factor.
- Extremely productive language once one knows the class methods.
- Clearly designed for use with a text editor and the command-line (It's great).
- The Elixir docs are usually the sole source of information, thankfully they are pretty good.

#### Moments of Joy
- [MacVim Vundle!](https://github.com/elixir-lang/vim-elixir)
- Using Interactive Elixir, `iex` and Mix is fantastic. Preferable to `sbt console`.
- Matching on assignment: `{:ok, result} = {:ok, 5}`.
- Functional style coupled with pipeline operators and anonymous methods makes for some beautiful code.
- `Stream.into` allows manipulation of infinite collections in a terse manner

    ```elixir
    output = File.stream!(output_file, [:delayed_write])
    stream = File.stream!(input_file)
              |> Stream.into(output, fn line -> map_line(line) end)
    Stream.run(stream)
    ```

#### Moments of Disappointment
- The lack of objects is initially infuriating. Hard to encapsulate logic, and structs don't seem like a substitute. It effectively means that most if not all built-in methods only return primitive types as opposed to objects.
- Lack of online resources because of small community. Few Stack Overflow posts, etc.
- Discoverability is tricky since methods are all class methods on primitive types.

## Conclusion

- Only after returning to a functional language like Elixir do I realize the convenience of **Object Oriented meets Functional** in Scala.
- The ability to return an object with relevant methods while still being immutable adds the power of discoverability, an advantage over the manipulation of maps and other primitives with Class methods.
- The big surprise was JRuby's performance and the impact of being able to use all cores. Running Puma on JRuby is very compelling when using a system with multiple cores.
- Golang's simplicity is very refreshing and their built-in profiling contributes to a philosophy of hand-tuning code for the best performance.
- Scala, on the other hand, has the user well removed from the low level, but the JVM handles a lot of optimizations for the programmer, and it shows. If only I didn't need an IDE...

For ETL operations, it would be remiss to ignore the Hadoop and Java ecosystem. Scala provides an incredible toolset for all ETL operations.

