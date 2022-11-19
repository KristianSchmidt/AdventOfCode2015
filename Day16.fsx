#load "Helpers.fsx"

open System
open System.Text.RegularExpressions

let ticker =
    [|
        "children: 3"
        "cats: 7"
        "samoyeds: 2"
        "pomeranians: 3"
        "akitas: 0"
        "vizslas: 0"
        "goldfish: 5"
        "trees: 3"
        "cars: 2"
        "perfumes: 1"
    |]
    |> Set.ofArray

let ans =
    Helpers.Web.getInput 16
    |> Array.map (fun s -> Regex.Replace(s, "Sue \d+: ", "").Split(", ") |> Set.ofArray)
    |> Array.mapi (fun i s -> i+1,Set.intersect ticker s |> Set.count)
    |> Array.maxBy snd
    |> fst

// Part 2

let rule (s : string) =
    let [|object; c|] = s.Split(' ')
    let count = int c
    match object with
    | "cats:" -> count > 7
    | "trees:" -> count > 3
    | "pomeranians:" -> count < 3
    | "goldfish:" -> count < 5
    | "children:" -> count = 3
    | "samoyeds:" -> count = 2
    | "akitas:" -> count = 0
    | "vizslas:" -> count = 0
    | "cars:" -> count = 2
    | "perfumes:" -> count = 1

let ans2 =
    Helpers.Web.getInput 16
    |> Array.map (fun s -> Regex.Replace(s, "Sue \d+: ", "").Split(", "))
    |> Array.mapi (fun i arr -> i+1, arr |> Array.forall rule)
    |> Array.find snd
    |> fst
