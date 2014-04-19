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

These tasks are not run on Hadoop but do run concurrently. Performance numbers are moot
since the CPU mostly sits idle waiting on Disk IO.

## The Languages:

1. Golang
2. Scala

### Golang

```
$ ./run_go
```

real  3m23.165s

user  2m56.002s

sys   0m26.542s
