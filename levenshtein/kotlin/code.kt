fun min(a: Int, b: Int, c: Int): Int = minOf(a, b, c)

fun levenshteinDistance(str1: String, str2: String): Int {
    val m = str1.length
    val n = str2.length
    val matrix = Array(m + 1) { IntArray(n + 1) }

    for (i in 0..m) {
        matrix[i][0] = i
    }
    for (j in 0..n) {
        matrix[0][j] = j
    }

    for (i in 1..m) {
        for (j in 1..n) {
            val cost = if (str1[i - 1] == str2[j - 1]) 0 else 1
            matrix[i][j] = min(
                matrix[i - 1][j] + 1,
                matrix[i][j - 1] + 1,
                matrix[i - 1][j - 1] + cost
            )
        }
    }

    return matrix[m][n]
}

fun main(args: Array<String>) {
    var minDistance = -1
    var times = 0

    for (i in args.indices) {
        for (j in args.indices) {
            if (i != j) {
                val distance = levenshteinDistance(args[i], args[j])
                if (minDistance == -1 || minDistance > distance) {
                    minDistance = distance
                }
                times++
            }
        }
    }

    println("times: $times")
    println("min_distance: $minDistance")
}