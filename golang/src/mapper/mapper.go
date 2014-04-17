package mapper

import(
  "os"
  "bufio"
  "fmt"
  "strings"
  "regexp"
)

func mapFunc(hood_id string, hood string, borough string, message string) {
    if match, err := regexp.MatchString("(?i)knicks", message); err == nil && match {
      fmt.Printf("%s\t%s\n", hood, "1")
    } else {
      fmt.Printf("%s\t%s\n", hood, "0")
    }
}

func Map(input string, output string) {
    // open input file
    file, err := os.Open(input)
    if err != nil { panic(err) }
    defer file.Close()

    // Can't use CsvReader because it is unforgiving with single quotes (")
    scanner := bufio.NewScanner(file)
    for scanner.Scan() {
      line := scanner.Text()
      record := strings.Split(line, "\t")

      hood_id, hood, borough, message := record[0], record[1], record[2], record[3]
      mapFunc(hood_id, hood, borough, message)
    }

    if err := scanner.Err(); err != nil {
      fmt.Fprintln(os.Stderr, "reading standard input:", err)
    }
}
