package jvm;

public class code {

    public static long levenshteinDistance(String s1, String s2) {
        int m = s1.length();
        int n = s2.length();
        long[] matrix = new long[(m + 1) * (n + 1)];

        // Initialize first row and column
        for (int i = 1; i <= m; i++) {
            matrix[i * (n + 1)] = i;
        }
        for (int j = 1; j <= n; j++) {
            matrix[j] = j;
        }

        // Compute Levenshtein distance
        for (int i = 0; i < m; i++) {
            for (int j = 0; j < n; j++) {
                long cost = (s1.charAt(i) == s2.charAt(j)) ? 0 : 1;
                long del = matrix[i * (n + 1) + (j + 1)] + 1;
                long ins = matrix[(i + 1) * (n + 1) + j] + 1;
                long sub = matrix[i * (n + 1) + j] + cost;
                matrix[(i + 1) * (n + 1) + (j + 1)] = Math.min(del, Math.min(ins, sub));
            }
        }

        return matrix[m * (n + 1) + n];
    }

    public static void main(String[] args) {
        if (args.length < 2) {
            System.out.println("Usage: java jvm.code <string1> <string2> ...");
            return;
        }

        long minDistance = -1;
        int times = 0;
        for (int i = 0; i < args.length - 1; i++) {
            for (int j = 0; j < args.length - 1; j++) {
                if (i != j) {
                    long distance = levenshteinDistance(args[i], args[j]);
                    if (minDistance == -1 || minDistance > distance) {
                        minDistance = distance;
                    }
                    times++;
                }
            }
        }

        // The only output from the program should be the times (number of comparisons)
        // and min distance calculated of all comparisons. Two total lines of output,
        // formatted exactly like this.
        System.out.println("times: " + times);
        System.out.println("min_distance: " + minDistance);
    }
}