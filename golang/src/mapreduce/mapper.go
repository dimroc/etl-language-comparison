package mapreduce

import (
	"bufio"
	"fmt"
	"io/ioutil"
	"os"
	"regexp"
	"strings"
)

func Map(inputDir string, output string) {
	inputFiles := filesInDir(inputDir)
	outputFiles := generateOutputFilenames(output, len(inputFiles))
	channel := make(chan bool, len(inputFiles))

	for index, inputPath := range inputFiles {
		outputPath := outputFiles[index]
		fmt.Printf("mapping from %s to %s\n", inputPath, outputPath)
		go mapSingleFileToChannel(inputPath, outputPath, channel)
	}

	// Opted for select over sync.WaitGroup to demonstrate unique Go features.
	// w: = sync.WaitGroup; w.Add(12); w.Wait()
	for _ = range inputFiles {
		select {
		case <-channel:
			fmt.Println("Finished mapping.")
		}
	}

	fmt.Println("DONE")
}

func mapSingleFileToChannel(inputPath string, outputPath string, channel chan bool) {
	mapSingleFile(inputPath, outputPath)
	channel <- true
}

func mapFunc(hood_id string, hood string, borough string, message string) string {
	if match, err := regexp.MatchString("(?i)knicks", message); err == nil && match {
		return fmt.Sprintf("%s\t%s\n", hood, "1")
	} else {
		return fmt.Sprintf("%s\t%s\n", hood, "0")
	}
}

func filesInDir(inputDir string) []string {
	files, err := ioutil.ReadDir(inputDir)
	if err != nil {
		panic(err)
	}

	filenames := make([]string, len(files))
	for i := 0; i < len(filenames); i++ {
		filenames[i] = fmt.Sprintf("%s/%s", inputDir, files[i].Name())
	}

	return filenames
}

func generateOutputFilenames(outputDir string, length int) []string {
	outputs := make([]string, length)
	for i := 0; i < length; i++ {
		outputs[i] = fmt.Sprintf("%s/output_%d", outputDir, i)
	}
	return outputs
}

func mapSingleFile(inputFile string, outputPath string) {
	// open input file
	file, err := os.Open(inputFile)
	if err != nil {
		panic(err)
	}
	defer file.Close()

	// open output file
	destination, err := os.Create(outputPath)
	if err != nil {
		panic(err)
	}
	defer destination.Close()

	// Can't use CsvReader because it is unforgiving with single quotes (")
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()
		record := strings.Split(line, "\t")

		hood_id, hood, borough, message := record[0], record[1], record[2], record[3]
		rval := mapFunc(hood_id, hood, borough, message)
		destination.WriteString(rval)
	}

	if err := scanner.Err(); err != nil {
		fmt.Fprintln(os.Stderr, "reading standard input:", err)
	}
}
