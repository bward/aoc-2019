#load "common.fsx"
#load "intcode.fsx"

open System.IO
open Common
open IntCode
 
let input = File.ReadLines("input\\9.txt") |> Seq.head |> (fun x -> x.Split(',')) |> Array.map bigint.Parse

let partOne input = run [2] input

let partTwo input = ""
 
input |> partOne |> printfn "%A"
input |> partTwo |> printfn "%O"