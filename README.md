## ETL Language Showdown
This repo implements the same map reduce ETL (Extract-Transform-Load) task in multiple languages
in an effort to compare language productivity, terseness and readability.

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
* Boy was that assumption wrong.

## The Languages

1. [Golang 1.2](http://golang.org/) - Imperative
2. [Scala 2.10.4](http://scala-lang.org/) - Both Imperative and Functional
3. [Elixir 0.13.0](http://elixir-lang.org/) - Functional

### Golang

```
$ ./run_go
```

#### Features used

- goroutines
- channels
- selects

#### Observations

- Performance after first write with no optimizations: `3m23.165s` (!!!)
- Only one core used despite spinning up multiple goroutines. [Had to research why all cores weren't used here](http://stackoverflow.com/questions/17868419/how-can-my-go-program-keep-all-the-cpu-cores-busy).
- Ultimately, [GOMAXPROCS will be removed](http://golang.org/pkg/runtime/#GOMAXPROCS)
- Performance average after setting GOMAXPROCS: `1m03.593s`
- Golang's libraries are fantastic but don't have the mature optimizations of other languages (yet).
- Ended up being the fewest lines of code across all languages, by a lot.

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
- Verbose error handling.
- Having to explicitly set the number of cores to use via `GOMAXPROCS` because of immature scheduling.
- Lack of collection helpers like `map` and `reduce`.

### Scala

```
$ ./run_scala
```

#### Features used

- Akka (Supervisors and Actors)

#### Observations

- Performance average after first write with no optimizations: [success] Total time: `34s`
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

#### Observations
- Performance average after first write with `:delayed_write`: `2m30.508s`
- Extremely productive language once one knows the class methods.
- Clearly designed for use with a text editor and the command-line (It's great).
- The Elixir docs are usually the sole source of information, thankfully they are pretty good.

#### Moments of Joy
- [MacVim Vundle!](https://github.com/elixir-lang/vim-elixir)
- Using Interactive Elixir, `iex` and Mix is fantastic. Perferable to `sbt console`.
- Matching on assignment: `{:ok, result} = {:ok, 5}`.
- Functional style coupled with pipeline operators and anonymous methods makes for some beautifully code.
- `Stream.into` allows manipulation of infinite collections in a terse manner

    ```elixir
    output = File.stream!(output_file, [:delayed_write])
    stream = File.stream!(input_file)
              |> Stream.into(output, fn line -> map_line(line) end)
    Stream.run(stream)
    ```

#### Moments of Disappointment
- The lack of objects is initially infuriating. Hard to encapsulate logic, and modules don't seem like a substitute. It effectively means that most if not all built-in methods only return primitive types as opposed to objects.
- Lack of online resources because of small community. Few Stack Overflow posts, etc.
- Discoverability is tricky since methods are all class methods on primitive types.
- Inability to fold/reduce from a stream in a straighforward manner. Had to hold contents in memory.


## Conclusion

Only after returning to a pure functional language like Elixir do I realize to convenience of **Object Oriented meets Functional**.

The ability to return an object with relevant methods while still being immutable adds the power of discoverability, a huge advantage over the manipulation of maps and other primitives with Class methods.

Golang's simplicity is very refreshing and their built-in profiling contributes to a philosophy of hand-tuning code for the best performance.
Scala, on the other hand, has the user well removed from the low level, but the JVM handles a lot of optimizations for the programmer, and it shows. If only I didn't need an IDE...

For ETL operations, it would be remiss to ignore the Hadoop and Java ecosystem. Scala provides an incredible toolset for all ETL operations.

