<?php

declare(strict_types=1);

function levenshtein_distance(string $str1, string $str2): int
{
    $m = strlen($str1);
    $n = strlen($str2);
  
    // Create a matrix to store distances
    $matrix = [];

    // Initialize first row and column
    for ($i = 0; $i <= $m; $i++) {
        $matrix[$i][0] = $i;
    }
    for ($j = 0; $j <= $n; $j++) {
        $matrix[0][$j] = $j;
    }
 
    // Compute Levenshtein distance
    for ($i = 1; $i <= $m; $i++) {
        for ($j = 1; $j <= $n; $j++) {
            $cost = (int) $str1[$i - 1] != $str2[$j - 1];
            $matrix[$i][$j] = min(
                $matrix[$i - 1][$j] + 1,        // Deletion
                $matrix[$i][$j - 1] + 1,        // Insertion
                $matrix[$i - 1][$j - 1] + $cost // Substitution
            );
        }
    }
  
  return $matrix[$m][$n];
}

// Main function to mimic C program's behavior
function main(array $args): void
{
    $minDistance = -1;
    $times = 0;

    foreach ($args as $i => $str1) {
        foreach ($args as $j => $str2) {
            if ($i != $j) {
                $distance = levenshtein_distance($str1, $str2);
                if ($minDistance == -1 || $minDistance > $distance) {
                    $minDistance = $distance;
                }

                $times++;
            }
        }
    }

    printf("times: %u\n", $times);
    printf("min_distance: %d\n", $minDistance);
}

// Skip script name.
unset($argv[0]);
// Call main.
main($argv);