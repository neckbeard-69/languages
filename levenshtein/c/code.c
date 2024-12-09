#include <stdio.h>
#include <string.h>
#include <stdlib.h>

int min(int a, int b, int c) {
  int min = a;
  if (b < min) min = b;
  if (c < min) min = c;
  return min;
}

// Compute Levenshtein distance between two strings
int levenshtein_distance(const char *str1, const char *str2) {
  int m = strlen(str1);
  int n = strlen(str2);
  
  // Create a matrix to store distances
  int matrix[m+1][n+1];
  
  // Initialize first row and column
  for (int i = 0; i <= m; i++) {
    matrix[i][0] = i;
  }
  for (int j = 0; j <= n; j++) {
    matrix[0][j] = j;
  }
 
  // Compute Levenshtein distance
  for (int i = 1; i <= m; i++) {
    for (int j = 1; j <= n; j++) {
      int cost = (str1[i-1] == str2[j-1]) ? 0 : 1;
      matrix[i][j] = min(
        matrix[i-1][j] + 1,      // Deletion
        matrix[i][j-1] + 1,      // Insertion
        matrix[i-1][j-1] + cost  // Substitution
      );
    }
  }
  
  return matrix[m][n];
}

int main(int argc, char *argv[]) {
  int min_distance = -1;
  int times = 0;
  for (int i = 0; i < argc-1; i++) {
    for (int j = 0; j < argc-1; j++) {
      if (i != j) {
        int distance = levenshtein_distance(argv[i+1], argv[j+1]);
        if (min_distance == -1 || min_distance > distance) {
          min_distance = distance;
        }
        times++;
      }
    }
  }
  
  // The only output from the program should be the times (number of comparisons) 
  // and min distance calculated of all comparisons. Two total lines of output, 
  // formatted exactly like this.
  printf("times: %d\n", times);
  printf("min_distance: %d\n", min_distance);
  
  return 0;
}
