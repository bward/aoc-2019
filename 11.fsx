#load "common.fsx"
#load "intcode.fsx"

open System.IO
open Common
open IntCode
 
let input = File.ReadLines("input\\11.txt") |> Seq.head |> (fun x -> x.Split(',')) |> Array.map int64

let directions = [| (1, 0); (0, 1); (-1, 0); (0, -1) |]

type Painted = 
    | Black
    | White

let getDirection direction move =
    let idx = Array.findIndex ((=) direction) directions
    if move = 1L
    then directions.[(idx + 1) % 4]
    else directions.[(idx + 3) % 4]

let rec paint pos painted state direction =
    let isWhite = Map.containsKey pos painted && Map.find pos painted = White
    let paintState = stepState {state with inputs = (if isWhite then 1L else 0L) :: state.inputs}

    match paintState with
    | Running s ->
        let turnDir = List.item 0 s.outputs
        let shouldPaint = List.item 1 s.outputs

        let newSquare = if shouldPaint = 1L then White else Black
        let newPainted = Map.add pos newSquare painted

        let newDirection = getDirection direction turnDir
        let newPos = (fst pos + fst newDirection, snd pos + snd newDirection)

        paint newPos newPainted s newDirection
    | Terminated _ -> painted


let partOne input =
    let startState = {idx = 0; instructions = Array.append (Array.copy input) (Array.create 1000 0L); outputs = []; inputs = []; relativeBase = 0}
    paint (0, 0) Map.empty startState (1, 0) |> Map.toList |> List.length

let partTwo input =
    let startState = {idx = 0; instructions = Array.append (Array.copy input) (Array.create 1000 0L); outputs = []; inputs = []; relativeBase = 0}
    paint (0, 0) (Map.add (0, 0) White Map.empty) startState (1, 0)
    |> Map.toList
    |> List.where (fun (p, c) -> c = White)
    |> List.map fst
    |> (fun coords ->
        let xMin = coords |> List.map fst |> List.min
        let xMax = coords |> List.map fst |> List.max
        let yMin = coords |> List.map snd |> List.min
        let yMax = coords |> List.map snd |> List.max
        [for y in yMin..yMax do yield [for x in xMin..xMax do yield if List.contains (x, y) coords then "*" else " "]]
    )
    |> List.map ((String.concat "") >> (printfn "%A"))

input |> partOne |> printfn "%A"
input |> partTwo