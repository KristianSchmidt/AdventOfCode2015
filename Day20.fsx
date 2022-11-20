#load "Helpers.fsx"

open System

let data = 29_000_000

let sumDivisors n =
    if (n % 10000 = 0) then
        printfn "n: %i" n
    let mutable sum = 0
    for i in 1 .. n/2 do
        let div = (double n) / (double i)
        let flo = Math.Floor((double n)/(double i))
        let sign = Math.Sign(div - flo)
        sum <- sum + i * ((sign + 1) % 2)

    n, (sum + n) * 10

sumDivisors 1000000

let ans = Seq.initInfinite (((+)600000) >> sumDivisors) |> Seq.find (snd >> fun x -> x > data)

// 702240 too high
// 665280 was ans

let sumDivisors2 n =
    if (n % 10000 = 0) then
        printfn "n: %i" n
    let mutable sum = 0
    for i in n/50 .. n/2 do
        let div = (double n) / (double i)
        let flo = Math.Floor((double n)/(double i))
        let sign = Math.Sign(div - flo)
        sum <- sum + i * ((sign + 1) % 2)

    n, (sum + n) * 11

let ans2 = Seq.initInfinite (((+)700000) >> sumDivisors2) |> Seq.find (snd >> fun x -> x > data)

// 705600 was answer