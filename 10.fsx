#load "common.fsx"

open System.IO
open Common
 
let input = File.ReadLines("input\\10.txt") |> Seq.map (Seq.map string)

let coords =
    [
        for y in 0..(input |> Seq.length)-1 do [
            for x in 0..(input |> Seq.head |> Seq.length)-1 do
                yield (x, y)
        ]
    ]
    |> List.collect id

let asteroids =
    coords |> List.where (fun (x, y) -> input |> Seq.item y |> Seq.item x |> (=) "#")
    
let angle (x1, y1) (x2, y2) =
    match (y1 - y2, x2 - x1) with
    | (x, y) when x >= 0 && y < 0 -> y ./. x |> atan |> (+) (2.0 * pi)
    | (x, y) when x < 0 -> y ./. x |> atan |> (+) pi
    | (x, y) -> y ./. x |> atan

let distance x y =
    match (x, y) with
    | ((x1, y1), (x2, y2)) -> (double (y2 - y1)) ** 2.0 + (double (x2 - x1)) ** 2.0 |> sqrt

let groups station =
    asteroids
    |> List.map (fun a -> (a, (distance station a, angle station a)))
    |> Seq.groupBy (snd >> snd)
    |> Seq.map (snd >> Seq.minBy (snd >> fst))
    |> Seq.sortBy (snd >> snd)
    |> List.ofSeq

let partOne =
    asteroids
    |> List.map (fun a -> (a, groups a |> List.length))
    |> List.maxBy snd

let partTwo =
    partOne
    |> fst
    |> groups
    |> Seq.item 200
    |> fst

partOne |> printfn "%A"
partTwo |> printfn "%A"