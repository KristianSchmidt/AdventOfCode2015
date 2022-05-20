open System
open System.IO
open System.Text.RegularExpressions

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

type Reindeer =
    { Name : string; Speed : int; Dist : int; Rest : int }

type State =
    | Resting of secondsLeft : int
    | Running of secondsLeft : int

let data =
    File.ReadAllLines(__SOURCE_DIRECTORY__ + "/input/day14.txt")
    |> Array.map (fun s ->
        match s with
        | Regex "(\\w+) can fly (\\d+) km/s for (\\d+) seconds, but then must rest for (\\d+) seconds."
                [name;speed;dist;rest] ->
                    { Name = name; Speed = int speed; Dist = int dist; Rest = int rest }
        )

let runRace timeLimit =
    let state =
        data
        |> Array.map (fun r -> r, (Running r.Dist, 0))
        |> Map.ofArray

    let rec f state points i =
        let nextIndividualState (r : Reindeer) (s,dist) =
            match s with
            | Running 1 -> (Resting r.Rest,dist+r.Speed)
            | Resting 1 -> (Running r.Dist,dist)
            | Running i -> (Running (i-1),dist+r.Speed)
            | Resting i -> (Resting (i-1),dist)

        let newState =
            state |> Map.map nextIndividualState

        let newPoints =
            let leaders =
                let maxDist = newState |> Map.toArray |> Array.map (snd >> snd) |> Array.max
                newState
                |> Map.toArray
                |> Array.where (snd >> snd >> ((=)maxDist))
                |> Array.map fst

            Array.concat [points; leaders]

        match i with
        | 0 -> newPoints
        | _ -> f newState newPoints (i-1)

    f state Array.empty timeLimit

let ans2 =
    runRace 2503
    |> Array.countBy id
    |> Array.maxBy snd
    |> snd
