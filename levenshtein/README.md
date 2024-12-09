# Levenshtein

This program computes the levenshtein distance between all of the strings provided on the command line.
It prints out the total number of strings compared for distance, and the lowest distance score of all comparisons.
This program emphasizes array/string access and basic looping and conditionals.

Below is the reference C program.
All languages must do the equivalent amount of work and meet these requirements:

```C
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
                                // ALL IMPLEMENTATIONS MUST...
int min(int a, int b, int c) {  // Have a function for calculating min
  int min = a;                  // If the language has a builtin or standard library min that supports 3+ inputs, that may be used as an alternative.
  if (b < min) min = b;
  if (c < min) min = c;
  return min;
}

int levenshtein(const char *str1, const char *str2) {  // A function that takes two string inputs, returns levenshtein distance
  int m = strlen(str1);                                // The lengths of the two strings must be ascertained somehow
  int n = strlen(str2);
  
  int **matrix = malloc((m + 1) * sizeof(int *));      // A MxN matrix must be allocated. Either stack or heap is acceptable.
  for (int i = 0; i <= m; i++) {
    matrix[i] = malloc((n + 1) * sizeof(int));
  }
  
  for (int i = 0; i <= m; i++) {                       // Matrix initialization step to generate first row and column.
    matrix[i][0] = i;
  }
  for (int j = 0; j <= n; j++) {
    matrix[0][j] = j;
  }
 
  for (int i = 1; i <= m; i++) {                       // Entire levenshtein matrix must be populated
    for (int j = 1; j <= n; j++) {                     // Using standard / naive levenshtein algorithm
      int cost = (str1[i-1] == str2[j-1]) ? 0 : 1;
      matrix[i][j] = min(matrix[i-1][j] + 1,
                         matrix[i][j-1] + 1,
                         matrix[i-1][j-1] + cost);
    }
  }
  
  int distance = matrix[m][n];                         // The matrix must be cleaned up.
  for (int i = 0; i <= m; i++) {                       // For a heap allocation this means some form of cleanup
    free(matrix[i]);                                   // If stack was used, should just clean up when returning
  }
  free(matrix);
  return distance;                                     // Return distance
}

int main(int argc, char *argv[]) {                     // Program accepts any number of string inputs on the command line
  int min_distance = -1;
  int times = 0;
  for (int i = 0; i < argc-1; i++) {                   // Compute levenshtein distance for all combinations of input strings
    for (int j = 0; j < argc-1; j++) {
      if (i != j) {                                    // Not including comparing a string with itself
        int distance = levenshtein(argv[i+1], argv[j+1]);
        if (min_distance == -1 || min_distance > distance) {
          min_distance = distance;                     // Keep track of the minimum distance
        }
        times++;                                       // Keep track of number of distance calls performed
      }
    }
  }
  
  printf("times: %d\n", times);                        // Print out count of distance calls performed
  printf("min_distance: %d\n", min_distance);          // Print out minimum distance
  
  return 0;
}
```
