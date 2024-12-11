def levenshtein_distance(str1: str, str2: str) -> int:
    m, n = len(str1), len(str2)
    
    # Create a matrix to store distances
    matrix = [[0] * (n + 1) for _ in range(m + 1)]
    
    # Initialize first row and column
    for i in range(m + 1):
        matrix[i][0] = i
    for j in range(n + 1):
        matrix[0][j] = j
    
    # Compute Levenshtein distance
    for i in range(1, m + 1):
        for j in range(1, n + 1):
            # Cost is 0 if characters match, 1 if they differ
            cost = 0 if str1[i-1] == str2[j-1] else 1
            matrix[i][j] = min(
                matrix[i-1][j] + 1,      # Deletion
                matrix[i][j-1] + 1,      # Insertion
                matrix[i-1][j-1] + cost  # Substitution
            )
    
    return matrix[m][n]

def main():
    import sys
    
    # Skip the first argument (script name)
    args = sys.argv[1:]
    
    min_distance = -1
    times = 0
    
    # Compare each pair of arguments exactly once
    for i in range(len(args)):
        for j in range(i+1, len(args)):
            if i != j:
                distance = levenshtein_distance(args[i], args[j])
                if min_distance == -1 or distance < min_distance:
                    min_distance = distance
                times += 1
    
    # The only output from the program should be the times (number of comparisons) 
    # and min distance calculated of all comparisons. Two total lines of output, 
    # formatted exactly like this.
    print(f"times: {times}")
    print(f"min_distance: {min_distance}")

if __name__ == "__main__":
    main()
