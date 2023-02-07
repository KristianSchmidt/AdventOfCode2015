#load "Helpers.fsx"

open System

let input = Helpers.Web.getInput 24 |> Array.map int

let len = input.Length
let target = input |> Array.sum |> (fun d -> d / 3)

let noDupes (arr : int array) =
    let mutable hasDupes = false
    for i in 0..arr.Length-2 do
        for j in i+1..arr.Length-1 do
            if (arr[i] = arr[j]) then hasDupes <- true
    not hasDupes

let getNrandoms t n =
    let arr =
        [|1..n|]
        |> Array.map (fun i -> input[Random.Shared.Next(0,len)])
    
    if (Array.sum arr = t) then
        Some arr
    else
        None

let ans1 =
    Seq.initInfinite (fun i -> i)
    |> Seq.choose (fun i -> getNrandoms target 6)
    |> Seq.filter noDupes
    |> Seq.take 1000
    |> Seq.distinct
    |> Seq.map (fun arr -> arr |> Array.map int64 |> Array.reduce (*))
    |> Seq.min

// Part 2

let target2 = input |> Array.sum |> (fun d -> d / 4)

let ans2 =
    Seq.initInfinite (fun i -> i)
    |> Seq.choose (fun i -> getNrandoms target2 5)
    |> Seq.filter noDupes
    |> Seq.take 1000
    |> Seq.distinct
    |> Seq.map (fun arr -> arr |> Array.map int64 |> Array.reduce (*))
    |> Seq.min



