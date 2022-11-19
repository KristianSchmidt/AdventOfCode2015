#load "Helpers.fsx"

let data = Helpers.Web.getInput 17 |> Array.map int |> Array.toList

let rec f remainder containers =
    // Either choose to keep or skip
    match containers with
    | x :: xs ->
        let keep =
            let newRemainder = remainder - x
            match newRemainder with
            | x when x < 0 -> 0
            | x when x = 0 -> 1
            | _ -> f newRemainder xs

        let skip = f remainder xs
        keep + skip
    | [] -> 0

f 150 data

// Part 2

let rec f' remainder containers cnt res =
    // Either choose to keep or skip
    match containers with
    | x :: xs ->
        let keep =
            let newRemainder = remainder - x
            match newRemainder with
            | x when x < 0 -> [0]
            | x when x = 0 -> [cnt+1]
            | _ -> f' newRemainder xs (cnt+1) res

        let skip = f' remainder xs cnt res
        List.concat [keep;skip;res]
    | [] -> res

f' 150 data 0 []
|> List.filter (fun x -> x > 0)
|> (fun x ->
    let min' = List.min x
    x |> List.filter ((=)min') |> List.length)

