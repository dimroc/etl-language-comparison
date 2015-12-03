package main

import (
	"bufio"
	"flag"
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"path/filepath"
	"regexp"
	"runtime"
	"runtime/pprof"
	"sort"
	"strings"
	"sync"
)

var (
	procs = flag.Int("procs", runtime.NumCPU(), "number of processors to use")
	// Assumes hyperthreading doubles # of cores
	maxprocs   = flag.Int("maxprocs", runtime.NumCPU()*2, "number of processors to use")
	input      = flag.String("in", "", "input directory")
	strategy   = flag.String("strategy", "regex", "query to search for")
	cpuprofile = flag.String("cpuprofile", "", "write cpu profile to file")
)

func check(err error) {
	if err != nil {
		log.Fatal(err)
	}
}

func generateOutput(stats []stat, output string) {
	os.MkdirAll(filepath.Dir(output), 0755)

	file, err := os.Create(output)
	if err != nil {
		panic(err)
	}
	defer file.Close()

	for _, hoodcount := range stats {
		rval := fmt.Sprintf("%s\t%d\n", hoodcount.Hood, hoodcount.Count)
		file.WriteString(rval)
	}
}

func main() {
	flag.Parse()
	if *cpuprofile != "" {
		f, err := os.Create(*cpuprofile)
		if err != nil {
			panic(err)
		}

		pprof.StartCPUProfile(f)
		defer pprof.StopCPUProfile()
	}

	if *input == "" {
		log.Fatal("input folder must be specified")
	}
	runtime.GOMAXPROCS(*maxprocs)

	matches := lengthMatcher(selectMatcher(*strategy))

	// queue of matched hoods.
	results := make(chan string, *maxprocs)

	// find files
	filelist, err := ioutil.ReadDir(*input)
	check(err)

	// parse and count
	var join sync.WaitGroup
	for _, file := range filelist {
		join.Add(1)
		go mapper(filepath.Join(*input, file.Name()), matches, results, &join)
	}
	go func() {
		join.Wait() // Rendezvous and cease the summarization.
		close(results)
	}()

	// merge results
	total := reducer(results)

	// sort stats
	stats := make([]stat, 0, len(total))
	for hood, count := range total {
		stats = append(stats, stat{hood, count})
	}
	sort.Sort(byCount(stats))

	// write to stdout
	generateOutput(stats, "tmp/golang_output")
}

func mapper(f string, matches matcher, results chan<- string, join *sync.WaitGroup) {
	defer join.Done()
	file, err := os.Open(f)
	check(err)
	defer file.Close()

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		record := strings.SplitN(scanner.Text(), "\t", 4)
		// id, hood, borough, message
		hood, message := record[1], record[3]
		if matches(message) {
			results <- hood
		}
	}
}

func reducer(results <-chan string) map[string]int {
	out := make(map[string]int)
	for hood := range results {
		out[hood]++
	}
	return out
}

type stat struct {
	Hood  string
	Count int
}

type byCount []stat

func (a byCount) Len() int      { return len(a) }
func (a byCount) Swap(i, j int) { a[i], a[j] = a[j], a[i] }
func (a byCount) Less(i, j int) bool {
	return a[i].Count > a[j].Count || (a[i].Count == a[j].Count && a[i].Hood < a[j].Hood)
}

type matcher func(string) bool

func lengthMatcher(next matcher) matcher {
	const l = len("knicks")
	return func(message string) bool {
		if len(message) < l {
			return false
		}
		return next(message)
	}
}

func selectMatcher(strategy string) matcher {
	if strategy == "substring" {
		return func(message string) bool {
			return strings.Contains(strings.ToLower(message), "knicks")
		}
	}
	reQuery := regexp.MustCompile("(?i)knicks")
	return func(message string) bool {
		return reQuery.MatchString(message)
	}
}
