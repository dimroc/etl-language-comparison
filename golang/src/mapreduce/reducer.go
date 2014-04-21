package mapreduce

import (
	"bufio"
	"fmt"
	"io/ioutil"
	"os"
	"sort"
	"strconv"
	"strings"
)

func Reduce(inputDir string, output string) {
	hash := make(map[string]int)

	files, err := ioutil.ReadDir(inputDir)
	if err != nil {
		panic(err)
	}

	for _, file := range files {
		if !file.IsDir() {
			path := fmt.Sprintf("%s/%s", inputDir, file.Name())
			fmt.Println("reducing file:", path)
			sumEntriesFromFile(path, hash)
		}
	}

	generateOutput(output, hash)
}

func sumEntriesFromFile(path string, hash map[string]int) {
	file, err := os.Open(path)
	if err != nil {
		panic(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()
		record := strings.Split(line, "\t")

		count, err := strconv.Atoi(record[1])
		if err != nil {
			panic(err)
		}

		if count > 0 {
			hash[record[0]]++
		}
	}
}

func generateOutput(output string, hash map[string]int) {
	file, err := os.Create(output)
	if err != nil {
		panic(err)
	}
	defer file.Close()

	sorted := convertToSortedTuples(hash)

	for _, hoodcount := range sorted {
		rval := fmt.Sprintf("%s\t%d\n", hoodcount.Hood, hoodcount.Count)
		file.WriteString(rval)
	}
}

func convertToSortedTuples(hash map[string]int) []hoodCount {
	var array byCount
	for key, value := range hash {
		array = append(array, hoodCount{key, value})
	}

	sort.Sort(array)
	return array
}

type hoodCount struct {
	Hood  string
	Count int
}

type byCount []hoodCount

func (a byCount) Len() int           { return len(a) }
func (a byCount) Swap(i, j int)      { a[i], a[j] = a[j], a[i] }
func (a byCount) Less(i, j int) bool { return a[i].Count > a[j].Count }
