package main

import (
	"bufio"
	"flag"
	"fmt"
	"io/ioutil"
	"io"
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
	procs    = flag.Int("procs", runtime.NumCPU(), "number of processors to use")
	maxprocs = flag.Int("maxprocs", runtime.NumCPU()*2, "number of processors to use")
	input    = flag.String("in", "", "input directory")
	output   = flag.String("out", "tmp/go_output", "input directory")
	query    = flag.String("query", "(?i)knicks", "query to search for")
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
	runtime.GOMAXPROCS(*maxprocs)

	reQuery := regexp.MustCompile(*query)

	// queues
	filenames := make(chan string, *procs)
	filtered := make(chan string, *procs)
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

	// parse and filter
	Spawn(*procs, func() {
		for filename := range filenames {
			file, err := os.Open(filename)
			outfilename := filepath.Join(*output, filepath.Base(filename))
			out, err := os.Create(outfilename)
			check(err)

			scanner := bufio.NewScanner(file)
			for scanner.Scan() {
				record := strings.Split(scanner.Text(), "\t")
				// id, hood, borough, message
				hood, message := record[1], record[3]
				if reQuery.MatchString(message) {
					out.WriteString(hood + "\t1\n")
				} else {
					out.WriteString(hood + "\t0\n")
				}
			}
			file.Close()
			out.Close()
			filtered <- outfilename
		}
	}, func() { close(filtered) })

	// reduce
	Spawn(*procs, func() {
		count := make(map[string]int)
		for filename := range filtered {
			file, err := os.Open(filename)
			check(err)

			rd := bufio.NewReader(file)
			for {
				line, _, err := rd.ReadLine()
				if err == io.EOF {
					break
				}
				check(err)
				record := strings.Split(string(line), "\t")
				// hood, matches
				hood, matches := record[0], record[1]
				if matches == "1" {
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
