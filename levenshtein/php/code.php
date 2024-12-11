<?php

declare(strict_types=1);

// Main function to mimic C program's behavior
function main(array $args): void
{
    $minDistance = -1;
    $times = 0;

    foreach ($args as $i => $str1) {
        foreach ($args as $j => $str2) {
            if ($i !== $j) {
                $distance = levenshtein($str1, $str2);
                if ($minDistance === -1 || $minDistance > $distance) {
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