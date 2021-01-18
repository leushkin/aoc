package main

import (
	"io/ioutil"
	"os"
	"strconv"
	"strings"
)

func main() {
	if len(os.Args) != 2 {
		panic("Usage: go run main.go <input_file>")
	}

	data, err := ioutil.ReadFile(os.Args[1])

	if err != nil {
		panic(err)
	}

	input := strings.Split(string(data), "\n")

	result := 0

	for _, line := range input {
		result += len(strconv.Quote(line)) - len(line)
	}

	print(result)
}
