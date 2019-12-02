#load "common.fsx"

open System.IO
open Common

let input = File.ReadLines("input\\2.txt") |> Seq.head |> (fun x -> x.Split(',')) |> Array.map int

let rec runProgram (idx: int) (input: int []) =
    let doOp op x y out = 
        Array.set input out (op (Array.get input x) (Array.get input y))
        runProgram (idx + 4) input
    match input.[idx..idx+3] with
        | [| 99; _; _; _ |] -> input.[0]  
        | [| 1; x; y; out |] -> doOp (+) x y out
        | [| 2; x; y; out |] -> doOp (*) x y out
        | _ -> invalidArg "input" "unrecognised op code"

let run noun verb input = 
    let copy = Array.copy input
    Array.set copy 1 noun
    Array.set copy 2 verb
    runProgram 0 copy

let search goal input =
    let nouns = [0 .. 99]
    let verbs = [0 .. 99]
    let pairs = nouns <*> verbs
    List.find (fun (n, v) -> run n v input = goal) pairs
        |> fun (n, v) -> 100 * n + v

let partOne = run 12 2
let partTwo = search 19690720
 
input |> partOne |> printfn "%O"
input |> partTwo |> printfn "%O"