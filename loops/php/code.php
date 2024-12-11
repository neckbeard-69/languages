<?php

declare(strict_types=1);

$u = (int) $argv[1];                   // Get an input number from the command line
$r = rand(0, 10_000);                  // Get a random number 0 <= r < 10k
$a = array_fill(0, 10_000, 0);         // Array of 10k elements initialized to 0
for ($i = 0; $i < 10_000; $i++) {      // 10k outer loop iterations
    for ($j = 0; $j < 100_000; $j++) { // 100k inner loop iterations, per outer loop iteration
        $a[$i] += $j % $u;             // Simple sum
    }
    $a[$i] += $r;                      // Add a random value to each element in array
}
echo $a[$r];                           // Print out a single element from the array
