package main

import "os"
import "./mapper"

func main() {
	if len(os.Args) <= 2 {
		panic("input dir and output dir command line arguments required\n")
	}

	inputDir := os.Args[1]
	outputDir := os.Args[2]

	mapper.Map(inputDir, outputDir)
}
