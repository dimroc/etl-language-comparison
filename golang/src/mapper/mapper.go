package mapper

import(
  "os"
  "bufio"
  "fmt"
  "strings"
  "regexp"
)

func mapFunc(hood_id string, hood string, borough string, message string) string {
    if match, err := regexp.MatchString("(?i)knicks", message); err == nil && match {
      return fmt.Sprintf("%s\t%s\n", hood, "1")
    } else {
      return fmt.Sprintf("%s\t%s\n", hood, "0")
    }
}

func Map(input string, output string) {
    // open input file
    file, err := os.Open(input)
    if err != nil { panic(err) }
    defer file.Close()

    // open output file
    destination, err := os.Create(output)
    if err != nil { panic(err) }
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
