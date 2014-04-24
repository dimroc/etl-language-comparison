package mapreduce

import (
	"bufio"
	"os"
	"strings"
	"testing"
)

func TestReduce(t *testing.T) {
	inputDir := "../../../fixtures"
	outputFile := "/tmp/golang_mapreduce_test_output/final"

	Reduce(inputDir, outputFile)

	file, err := os.Open(outputFile)
	if err != nil {
		panic(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	scanner.Scan()
	line := scanner.Text()
	tokens := strings.Split(line, "\t")

	if len(tokens) != 2 {
		t.Fatal("Incorrect number of output columns")
	}

	if tokens[1] != "2" {
		t.Fatal("Incorrect aggregation")
	}
}
