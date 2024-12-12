#include <algorithm>
#include <iostream>
#include <string_view>
#include <vector>

using namespace std;

int levenshtein(const string_view &str1, const string_view &str2) {
  int n = str1.size();
  int m = str2.size();

  vector<vector<int>> matrix(m + 1, vector<int>(n + 1));

  for (int i = 0; i <= m; i++) {
    matrix[i][0] = i;
  }

  for (int j = 0; j <= n; j++) {
    matrix[0][j] = j;
  }

  for (int i = 1; i <= m; i++) {
    for (int j = 1; j <= n; j++) {
      int cost = (str1[i - 1] == str2[j - 1] ? 0 : 1);
      matrix[i][j] = min({matrix[i - 1][j] + 1, matrix[i][j - 1] + 1,
                          matrix[i - 1][j - 1] + cost});
    }
  }

  return matrix[m][n];
}

int main(int argc, char *argv[1]) {
  int min_distance = -1;
  int times = 0;
  for (int i = 1; i < argc; i++) {
    for (int j = 1; j < argc; j++) {
      if (i != j) {
        int distance = levenshtein(argv[i], argv[j]);
        if (min_distance == -1 || min_distance > distance) {
          min_distance = distance;
        }
        times++;
      }
    }
  }

  cout << "times: " << times << '\n';
  cout << "min_distance: " << min_distance << '\n';

  return 0;
}
