#load "Helpers.fsx"

open System
open Helpers

let instr = Helpers.Web.getInput 23

let rec processInstr idx (a : int) (b : int) =
    //printfn "Idx: %i a=%i b=%i" idx a b
    
    if (idx < 0 || idx >= instr.Length) then b else
    match instr[idx] with
    | Regex "hlf ([ab])" [r] ->
        match r with
        | "a" -> processInstr (idx+1) (a / 2) b
        | "b" -> processInstr (idx+1) a (b / 2)
    | Regex "tpl ([ab])" [r] -> 
        match r with
        | "a" -> processInstr (idx+1) (a * 3) b
        | "b" -> processInstr (idx+1) a (b * 3)
    | Regex "inc ([ab])" [r] ->
        match r with
        | "a" -> processInstr (idx+1) (a + 1) b
        | "b" -> processInstr (idx+1) a (b + 1)
    | Regex "jmp ([+-])(\d+)" [pm;d] ->
        match pm with
        | "+" -> processInstr (idx+(int d)) a b
        | "-" -> processInstr (idx-(int d)) a b
    | Regex "jie ([ab]), ([+-])(\d+)" [r;pm;d] ->
        let reg = match r with | "a" -> a | "b" -> b
        if (reg % 2 = 0) then
            match pm with
            | "+" -> processInstr (idx+(int d)) a b
            | "-" -> processInstr (idx-(int d)) a b
        else
            processInstr (idx+1) a b
    | Regex "jio ([ab]), ([+-])(\d+)" [r;pm;d] ->
        let reg = match r with | "a" -> a | "b" -> b
        if (reg = 1) then
            match pm with
            | "+" -> processInstr (idx+(int d)) a b
            | "-" -> processInstr (idx-(int d)) a b
        else
            processInstr (idx+1) a b

let ans1 = processInstr 0 0 0
let ans2 = processInstr 0 1 0



