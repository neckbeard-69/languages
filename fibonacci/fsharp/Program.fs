open System

let rec fibonacci n =
    match n with
    | 0 -> 0
    | 1 -> 1
    | _ -> fibonacci (n - 1) + fibonacci (n - 2)

[<EntryPoint>]
let main argv =
    if argv.Length < 1 then
        printfn ""
        1
    else
        let u = int argv.[0]
        let mutable r = 0
        for i in 1 .. (u - 1) do
            r <- r + fibonacci i
        printfn "%d" r
        0