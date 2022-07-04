#load "Helpers.fsx"

open System
open System.Text.RegularExpressions

let data =
    Helpers.Web.getInput 15
    |> Array.map (fun s -> Regex.Replace(s, "[^\d-,]", "").Split(',') |> Array.map int)
    |> Array.map (fun [|c;d;f;t;cal|] -> {| Capacity = c; Durability = d; Flavor = f; Texture = t; Calories = cal |})

let calcScore i j k l =
    let cap = data[0].Capacity * i + data[1].Capacity * j + data[2].Capacity * k + data[3].Capacity * l
    let dur = data[0].Durability * i + data[1].Durability * j + data[2].Durability * k + data[3].Durability * l
    let fla = data[0].Flavor * i + data[1].Flavor * j + data[2].Flavor * k + data[3].Flavor * l
    let tex = data[0].Texture * i + data[1].Texture * j + data[2].Texture * k + data[3].Texture * l
    let cal = data[0].Calories * i + data[1].Calories * j + data[2].Calories * k + data[3].Calories * l
    if (cap < 0 || dur < 0 || fla < 0 || tex < 0 (*|| cal < 0*) ) then
        0L,cal
    else
        (int64 cap) * (int64 dur) * (int64 fla) * (int64 tex),cal// * (int64 cal)

// Part 1

let mutable maxScore = 0L
for i in 0..100 do
    for j in 0 .. 100 - i do
        for k in 0 .. 100 - (i+j) do
            for l in 0 .. 100 - (i+j+k) do
                let (score,_) = calcScore i j k l
                maxScore <- max maxScore score
                
// Part 2

let mutable maxScore2 = 0L
for i in 0..100 do
    for j in 0 .. 100 - i do
        for k in 0 .. 100 - (i+j) do
            for l in 0 .. 100 - (i+j+k) do
                let (score,cal) = calcScore i j k l
                if (cal = 500) then
                    maxScore2 <- max maxScore2 score
