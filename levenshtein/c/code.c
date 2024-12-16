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
int levenshtein_distance(const char *str1t, const char *str2t) {
  int mt = strlen(str1t);
  int nt = strlen(str2t);
  const char* str1 = mt <= nt ? str1t : str2t;
  const char* str2 = mt <= nt ? str2t : str1t;
  int m = strlen(str1);
  int n = strlen(str2);
  
  int prev[m+1];
  int curr[m+1];
  
  for (int i = 0; i <= m; i++) {
    prev[i] = i;
  }
 
  // Compute Levenshtein distance
  for (int i = 1; i <= n; i++) {
    curr[0] = i;
    for (int j = 1; j <= m; j++) {
      int cost = (str1[j-1] == str2[i-1]) ? 0 : 1;
      curr[j] = min(
        prev[j] + 1,      // Deletion
        curr[j-1] + 1,      // Insertion
        prev[j-1] + cost  // Substitution
      );
    }
    for (int j = 0; j <= m; j++) {
      prev[j] = curr[j];
    }
  }
  
  return prev[m];
}

int main(int argc, char *argv[]) {
  int min_distance = -1;
  int times = 0;
  for (int i = 1; i < argc; i++) {
    for (int j = 1; j < argc; j++) {
      if (i != j) {
        int distance = levenshtein_distance(argv[i], argv[j]);
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
