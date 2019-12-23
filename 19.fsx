#load "common.fsx"
#load "intcode.fsx"

open System.IO
open Common
open IntCode

let input = File.ReadLines("input\\19.txt") |> Seq.head |> (fun x -> x.Split(',')) |> Array.map int64

let partOne input =
    let inputs = [for x in 0L..49L do yield! [for y in 0L..49L do yield [x; y]]]
    inputs
    |> List.sumBy (fun coords ->
        run coords input
        |> (fun s -> List.head s.outputs)
    )

let startOfRow input y =
    List.find (fun x -> ((run [x; y] input).outputs |> List.head) = 1L) [0L..9999L]

let squareAtY input startY = 
    let startX = startOfRow input startY
    [for y in (startY-99L)..startY do yield [startX; y]; for x in startX..(startX+99L) do yield [x; startY-99L]]
    |> List.map (fun coords -> ((run coords input).outputs |> List.head) = 1L)
    |> List.forall id 

let partTwo input =
    let y = binarySearch (squareAtY input) 99L 10000L - 99L
    let x = startOfRow input y
    x + y
    
input |> partOne |> printfn "%A"
input |> partTwo |> printfn "%A"