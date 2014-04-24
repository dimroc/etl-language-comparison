## ETL Language Showdown
This repo implements the same map reduce ETL (Extract-Transform-Load) task in multiple languages
in an effort to compare language productivity, terseness and readability.

## The Task:
Count the number of tweets that mention 'knicks' in their message and bucket based on the neighborhood of origin.
The ~1GB dataset for this task, sampled below, contains a tweet's message and its NYC neighborhood. [It can be downloaded here](https://dimroc-public.s3.amazonaws.com/etl-language-comparison/tweets20140416.tar.gz).

```
91	west-brighton	Brooklyn	Uhhh
121	turtle-bay-east-midtown	Manhattan	Say anything
175	morningside-heights	Manhattan	It feels half-cheating half-fulfilling to cite myself.
```

### Initial Assumption:

* These tasks are not run on Hadoop but do run concurrently. Performance numbers are moot since the CPU mostly sits idle waiting on Disk IO.
* Boy was that assumption wrong.

## The Languages:

1. Golang 1.2
2. Scala 2.10.3

### Golang

```
$ ./run_go
```

#### Features used:

- goroutines
- channels
- selects

#### Observations:

- Performance after first write with no optimizations: `3m23.165s`
- Only one core used despite spinning up multiple goroutines. [Had to research why all cores weren't used here](http://stackoverflow.com/questions/17868419/how-can-my-go-program-keep-all-the-cpu-cores-busy).
- After setting GOMAXPROCS, performance dropped to: `1m03.593s`
- Ultimately, [GOMAXPROCS will be removed](http://golang.org/pkg/runtime/#GOMAXPROCS)
- Golang's libraries are fantastic but don't have the mature optimizations of other languages (yet).

### Scala

#### Features used:

- Akka (Supervisors and Actors)

#### Observations:

- Performance after first write with no optimizations: [success] Total time: `40s`
- All cores used.
- Not as IO bound as originally thought. Attributed to the optimizations in the BufferedSource/BufferedWriter classes.


