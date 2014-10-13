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
	"sort"
	"strings"
	"sync/atomic"
)

var (
	procs = flag.Int("procs", runtime.NumCPU(), "number of processors to use")
	input = flag.String("in", "", "input directory")
	query = flag.String("query", "(?i)knicks", "query to search for")
)

func check(err error) {
	if err != nil {
		log.Fatal(err)
	}
}

func main() {
	flag.Parse()
	if *input == "" {
		log.Fatal("input folder must be specified")
	}
	runtime.GOMAXPROCS(runtime.NumCPU() * 2)

	reQuery := regexp.MustCompile(*query)

	// queues
	filenames := make(chan string, *procs)
	results := make(chan map[string]int, *procs)

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
	Spawn(*procs, func() {
		count := make(map[string]int)
		for filename := range filenames {
			file, err := os.Open(filename)
			check(err)

			scanner := bufio.NewScanner(file)
			for scanner.Scan() {
				record := strings.Split(scanner.Text(), "\t")
				// id, hood, borough, message
				hood, message := record[1], record[3]
				if reQuery.MatchString(message) {
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
	for _, stat := range stats {
		fmt.Println(stat.Hood, "\t", stat.Count)
	}
}

type Stat struct {
	Hood  string
	Count int
}

type byCount []Stat

func (a byCount) Len() int           { return len(a) }
func (a byCount) Swap(i, j int)      { a[i], a[j] = a[j], a[i] }
func (a byCount) Less(i, j int) bool { return a[i].Count > a[j].Count }

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
