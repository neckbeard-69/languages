# base case 1
fibonaci(::Val{0}) = 0
# base case 2
fibonaci(::Val{1}) = 1
# general case
fibonaci(::Val{n}) where n = fibonaci(Val(n-1)) + fibonaci(Val(n-2))

let
    u = parse(Int,ARGS[1])
    r = 0
    for i ∈ 1:u
        r += fibonaci(Val(i))
    end
    println(r)
end