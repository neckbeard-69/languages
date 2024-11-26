let u = UInt(CommandLine.arguments[1])!  // Get input number from command line
let r = UInt.random(in: 0..<10000)  // Get random number 0 <= r < 10k
var a = Array(repeating: 0, count: 10000)  // Array of 10k elements initialized to 0

for i in 0..<10000 {  // 10k outer loop iterations
    for j in 0..<100000 {  // 100k inner loop iterations
        a[i] += j % Int(u)  // Simple sum
    }
    a[i] += Int(r)  // Add random value to each element
}

print(a[Int(r)])  // Print single element from array
