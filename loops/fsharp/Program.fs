open System

[<EntryPoint>]
let main argv =
    if argv.Length < 1 then
        printfn ""
        1
    else
        let u = int argv.[0]
        let rnd = Random()
        let r = rnd.Next(10000)
        let a = Array.zeroCreate<int> 10000
        for i in 0..9999 do
            for j in 0..99999 do
                a.[i] <- a.[i] + j % u
            a.[i] <- a.[i] + r
        printfn "%d" a.[r]
        0