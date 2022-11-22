#load "Helpers.fsx"

open System

let data = Helpers.Web.getInput 19

let start = data |> Array.last

let rules =
    data[0..data.Length-3]
    |> Array.map (Helpers.split " => ")
    |> Array.groupBy Array.head
    |> Array.map (fun (s,arr) -> s,arr |> Array.map (Array.tail >> Array.head))
    |> Map.ofArray
    
//let reps = rules |> Array.map fst

let startsWithAt (s : string) idx (startsWith : string) =
    s[idx..].StartsWith(startsWith) 

//rules |> Array.map fst |> Array.iter (printfn "%s")

startsWithAt start 0 "CR"

let keys =
    rules
    |> Map.keys
    |> Seq.toArray

let isMatchAt idx =
    keys
    |> Array.tryFind (startsWithAt start idx)
    |> Option.map (fun c -> idx,c)

let replaceAt idx (prev : string) (next : string) =
    start.Substring(0,idx) + next + start.Substring(idx+prev.Length)

let ans1 =
    [0..start.Length-1]
    |> List.choose isMatchAt
    |> List.collect (fun (i,prev) -> rules[prev] |> List.ofArray |> List.map (replaceAt i prev))
    |> Set.ofList
    |> Set.count

