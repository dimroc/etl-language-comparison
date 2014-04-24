package main

import (
	"./mapreduce"
	"fmt"
	"os"
	"runtime"
)

func main() {
	if len(os.Args) <= 2 {
		panic("input dir and output dir command line arguments required\n")
	}

	goProcs := runtime.NumCPU()
	fmt.Printf("Setting maximum number of CPUs to %d\n", goProcs)
	runtime.GOMAXPROCS(goProcs)

	inputDir := os.Args[1]
	outputDir := os.Args[2]
	finalOutput := fmt.Sprintf("%s/final/final", outputDir)

	mapreduce.Map(inputDir, outputDir)
	mapreduce.Reduce(outputDir, finalOutput)
}
