package mapreduce

import (
	"bufio"
	"fmt"
	"io/ioutil"
	"os"
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
		path := fmt.Sprintf("%s/%s", inputDir, file.Name())
		fmt.Println("reducing file:", path)
		sumEntriesFromFile(path, hash)
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

	for key, value := range hash {
		rval := fmt.Sprintf("%s\t%d\n", key, value)
		file.WriteString(rval)
	}
}
