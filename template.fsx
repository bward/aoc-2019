#load "common.fsx"

open System.IO
open Common
 
let input = File.ReadLines("input\\7.txt") |> Seq.head |> (fun x -> x.Split(',')) |> List.ofArray |> List.map int

let partOne input = input

let partTwo input = ""
 
input |> partOne |> printfn "%O"
input |> partTwo |> printfn "%O"