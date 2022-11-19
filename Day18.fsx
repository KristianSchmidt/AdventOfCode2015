#load "Helpers.fsx"

let data =
    Helpers.Web.getInput 18
    |> Array.map (fun s -> s.ToCharArray())
    |> Helpers.toGridMap

let neighbors (x,y) = [(x-1,y-1); (x, y-1); (x+1, y-1)
                       (x-1,y);             (x+1, y)
                       (x-1,y+1); (x, y+1); (x+1, y+1)]

let nFiltered (x,y) =
    neighbors (x,y)
    |> List.filter
        (fun (x,y) -> match (x,y) with
                      | (x,y) when x >= 0 && x <= 99 && y >= 0 && y <= 99 -> true
                      | _ -> false)

let doIter map =
    map
    |> Map.map (fun k v ->
        let ons = nFiltered k |> List.filter (fun n -> Map.find n map = '#') |> List.length
        let offs = nFiltered k |> List.filter (fun n -> Map.find n map = '.') |> List.length
        match v with
        | '#' -> if (ons = 2 || ons = 3) then '#' else '.'
        | '.' -> if (ons = 3) then '#' else '.'
    )

let repeat n fn = List.init n (fun _ -> fn) |> List.reduce (>>)

let res = repeat 100 (fun m -> doIter m) data

let ans = res |> Map.filter (fun k v -> v = '#') |> Map.count

// Part 2

let doIter' map =
    map
    |> Map.map (fun k v ->
        let ons = nFiltered k |> List.filter (fun n -> Map.find n map = '#') |> List.length
        let offs = nFiltered k |> List.filter (fun n -> Map.find n map = '.') |> List.length
        match k, v with
        | (0,0),_ -> '#'
        | (99,99),_ -> '#'
        | (0,99),_ -> '#'
        | (99,0),_ -> '#'
        | _, '#' -> if (ons = 2 || ons = 3) then '#' else '.'
        | _,'.' -> if (ons = 3) then '#' else '.'
    )

let newMap =
    data
    |> Map.add (0,0) '#'
    |> Map.add (0,99) '#'
    |> Map.add (99,99) '#'
    |> Map.add (99,0) '#'

let res2 = repeat 100 (fun m -> doIter' m) newMap

let ans2 = res2 |> Map.filter (fun k v -> v = '#') |> Map.count
