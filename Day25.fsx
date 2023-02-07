#load "Helpers.fsx"

open System
open Helpers

let (row,col) =
    match Helpers.Web.getInput 25 |> Array.head with
    | Regex ".+row (\d+), column (\d+)\." [r;c] -> int r, int c

let rec find (currRow,currCol) maxRow (currVal : int64) =
    //printfn "(%i,%i) = %i" currRow currCol currVal
    if (currRow = row && currCol = col) then
        currVal
    else
        let nextRow,nextMaxRow = if (currRow = 1) then maxRow + 1,maxRow + 1 else currRow - 1,maxRow
        let nextCol = if (currRow = 1) then 1 else currCol + 1
        let nextVal = (currVal * 252533L) % 33554393L
        find (nextRow, nextCol) nextMaxRow nextVal

find (1,1) 1 20151125L
