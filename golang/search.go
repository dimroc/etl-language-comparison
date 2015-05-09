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
	"sync/atomic"
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

func writeToStdout(stats []Stat) {
	for _, stat := range stats {
		fmt.Println(stat.Hood, "\t", stat.Count)
	}
}

func generateOutput(stats []Stat, output string) {
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

	matches := selectMatcher(*strategy)

	// queues
	filenames := make(chan string)
	results := make(chan map[string]int)

	// find files
	go func() {
		filelist, err := ioutil.ReadDir(*input)
		check(err)
		for _, file := range filelist {
			filenames <- filepath.Join(*input, file.Name())
		}
		close(filenames)
	}()

	// parse and count
	Spawn(*maxprocs, func() {
		count := make(map[string]int)
		for filename := range filenames {
			file, err := os.Open(filename)
			check(err)

			scanner := bufio.NewScanner(file)
			for scanner.Scan() {
				record := strings.Split(scanner.Text(), "\t")
				// id, hood, borough, message
				hood, message := record[1], record[3]
				if matches(message) {
					count[hood]++
				}
			}

			file.Close()
		}
		results <- count
	}, func() { close(results) })

	// merge results
	total := make(map[string]int)
	for result := range results {
		for hood, count := range result {
			total[hood] += count
		}
	}

	// sort stats
	stats := make([]Stat, 0, len(total))
	for hood, count := range total {
		stats = append(stats, Stat{hood, count})
	}
	sort.Sort(byCount(stats))

	// write to stdout
	//writeToStdout(stats)
	generateOutput(stats, "tmp/golang_output")
}

type Stat struct {
	Hood  string
	Count int
}

type byCount []Stat

func (a byCount) Len() int      { return len(a) }
func (a byCount) Swap(i, j int) { a[i], a[j] = a[j], a[i] }
func (a byCount) Less(i, j int) bool {
	return a[i].Count > a[j].Count || (a[i].Count == a[j].Count && a[i].Hood < a[j].Hood)
}

// Spawns N routines, after each completes runs all whendone functions
func Spawn(N int, fn func(), whendone ...func()) {
	waiting := int32(N)
	for k := 0; k < N; k += 1 {
		go func() {
			fn()
			if atomic.AddInt32(&waiting, -1) == 0 {
				for _, fn := range whendone {
					fn()
				}
			}
		}()
	}
}

type matcher func(string) bool

func selectMatcher(strategy string) matcher {
	if strategy == "substring" {
		return func(message string) bool {
			return strings.Contains(strings.ToLower(message), "knicks")
		}
	} else {
		reQuery := regexp.MustCompile("(?i)knicks")
		return func(message string) bool {
			return reQuery.MatchString(message)
		}
	}
}
