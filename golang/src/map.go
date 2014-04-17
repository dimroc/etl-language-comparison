package main

import "os"
import "fmt"
import "./mapper"

func main() {
	// read whole the file
	if len(os.Args) <= 2 {
		panic("input and output command line arguments required\n")
	}

	input := os.Args[1]
	output := os.Args[2]
	fmt.Printf("mapping from %s to %s\n", input, output)

	mapper.Map(input, output)
}
