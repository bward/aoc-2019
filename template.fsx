#load "common.fsx"

open System.IO
open Common
 
let input = File.ReadLines("input\\1.txt")

let partOne input = ""

let partTwo input = ""
 
input |> partOne |> printfn "%O"
input |> partTwo |> printfn "%O"