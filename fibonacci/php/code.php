<?php

declare(strict_types=1);

function fibonacci(int $n): int                   // Have a function that recursively compute a fibonacci number with this naive algorithm
{
    if ($n <= 1) {                                // Base case for input 0, 1.
    	return $n;
    }

    return fibonacci($n - 1) + fibonacci($n - 2); // Must make two recursive calls for each non-base invocation
}

$u = (int) $argv[1];                              // Get exactly one numeric value from the command line
$r = 0;                                           // Create variable to store sum
for ($i = 1; $i < $u; $i++) {                     // Loop 1...u times
    $r += fibonacci($i);                          // Sum all fibonacci numbers 1...u
}
echo $r;                                          // Print out the single, numeric sum
