package main

import (
	"fmt"
	"os"
)

func min(a, b, c int) int  {
	min := a
	if b < min {
		min = b
	} else {
		min = c
	}

	return min
}

func levenshtein(s1, s2 string) int {
	m := len(s1)
	n := len(s2)

	matrix := make([][]int, m+1)
	for i := range matrix {
		matrix[i] = make([]int, n+1)
	}

	for i := 0; i <= m; i++ {
		matrix[i][0] = i
	}
	for j := 0; j <= n; j++ {
		matrix[0][j] = j
	}

	for i := 1; i <= m; i++ {
		for j := 1; j <= n; j++ {
			cost := 0
			if s1[i-1] == s2[j-1] {
				cost = 1
			}
			matrix[i][j] = min(
				matrix[i-1][j]+1,
				matrix[i][j-1]+1,
				matrix[i-1][j-1]+cost,
			)
		}
	}
	return matrix[m][n]
}

func main() {
	args := os.Args[1:]

	minD := -1
	times := 0

	if len(args) < 2 {
		fmt.Println("Please provide at least two strings as arguments.")
	}

	for i := 0; i < len(args); i++ {
		for j := i + 1; j < len(args); j++ {
			d := levenshtein(args[i], args[j])
			if minD == -1 || d < minD {
				minD = d
			}
			times++
		}
	}

	fmt.Printf("times: %d\n", times)
	fmt.Printf("min_distance: %d\n", minD)
}
