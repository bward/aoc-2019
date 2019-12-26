#load "common.fsx"
#load "intcode.fsx"

open System.IO
open System
open Common
open IntCode

let input = File.ReadLines("input\\25.txt") |> Seq.head |> (fun x -> x.Split(',')) |> Array.map int64

let partOne input =
    let start = initState input []
    let rec run state = 
        let stepped = stepState state |> getState
        stepped.outputs |> List.rev |> List.map (char >> string) |> String.concat "" |> printfn "%A"
        let inputString = Console.ReadLine()
        let input = inputString |> Seq.map int64 |> (fun s -> List.ofSeq s @ [10L])
        run {stepped with inputs = input; outputs = []}
    run start


let partTwo input = ""
 
input |> partOne |> printfn "%O"
input |> partTwo |> printfn "%O"