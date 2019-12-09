#load "common.fsx"
#load "intcode.fsx"

open System.IO
open Common
open IntCode
 
let input = File.ReadLines("input\\9.txt") |> Seq.head |> (fun x -> x.Split(',')) |> Array.map int64

let partOne input = run [1L] input

let partTwo input = run [2L] input
 
input |> partOne |> printfn "%A"
input |> partTwo |> printfn "%A"