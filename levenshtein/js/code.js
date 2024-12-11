function min(a, b, c) {
  let minVal = a;
  if (b < minVal) minVal = b;
  if (c < minVal) minVal = c;
  return minVal;
}

function levenshteinDistance(str1, str2) {
  const m = str1.length;
  const n = str2.length;
  
  const matrix = new Array(m + 1).fill(0).map(() => new Array(n + 1).fill(0));
  
  for (let i = 0; i <= m; i++) {
    matrix[i][0] = i;
  }
  for (let j = 0; j <= n; j++) {
    matrix[0][j] = j;
  }
 
  // Compute Levenshtein distance
  for (let i = 1; i <= m; i++) {
    for (let j = 1; j <= n; j++) {
      const cost = (str1[i-1] === str2[j-1]) ? 0 : 1;
      matrix[i][j] = min(
        matrix[i-1][j] + 1,      // Deletion
        matrix[i][j-1] + 1,      // Insertion
        matrix[i-1][j-1] + cost  // Substitution
      );
    }
  }
  
  // Return the final distance
  return matrix[m][n];
}

// Main function to mimic C program's behavior
function main(args) {
  let minDistance = -1;
  let times = 0;
  
  for (let i = 0; i < args.length; i++) {
    for (let j = 0; j < args.length; j++) {
      if (i !== j) {
        const distance = levenshteinDistance(args[i], args[j]);
        if (minDistance === -1 || minDistance > distance) {
          minDistance = distance;
        }
        times++;
      }
    }
  }
  
  // Output exactly like the C program
  console.log(`times: ${times}`);
  console.log(`min_distance: ${minDistance}`);
  
  return 0;
}

// Handle command-line arguments for Node.js
if (typeof process !== 'undefined' && process.argv) {
  // Skip first two args (node executable and script path)
  const args = process.argv.slice(2);
  main(args);
}
