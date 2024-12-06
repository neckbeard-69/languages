tailrec fun fibonacci(
    n: Int, a: Int = 0, b: Int = 1
): Int = when (n) {
    0 -> a; 1 -> b; else -> fibonacci(n - 1, b, a + b)
}

fun main(args: Array<String>) {
    val u: Int = args.get(0).toInt()
    var r: Int = 0
    for (i in 1 until u) {
        r += fibonacci(i)
    }
    println(r)
}

