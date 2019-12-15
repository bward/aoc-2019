#load "common.fsx"
#load "intcode.fsx"

open System.IO
open Common
open IntCode
 
let input = File.ReadLines("input\\13.txt") |> Seq.head |> (fun x -> x.Split(',')) |> Array.map int64

let drawScreen outputs =
    let rec draw screen coords =
        match coords with
        | x :: y :: t :: rest -> draw (Map.add (x, y) t screen) rest
        | _ -> screen
    draw Map.empty (List.rev outputs)
 
let partOne input =
    let outputs = (run [] input).outputs
    drawScreen outputs
    |> Map.filter (fun _ t -> t = 2L)
    |> Map.count

let partTwo (input: array<int64>) =
    let startState = {idx = 0; instructions = Array.concat [ [|2L|]; (Array.copy input.[1..]); (Array.create 1000 0L)]; inputs = []; outputs = []; relativeBase = 0}
    let rec run state =
        match stepState state with
        | Running r ->
            let screen = drawScreen r.outputs
            let paddleX = Map.findKey (fun _ v -> v = 3L) screen |> fst
            let ballX = Map.findKey (fun _ v -> v = 4L) screen |> fst
            let joystick = compare ballX paddleX |> int64
            run {state with inputs = [joystick]} 
        | Terminated t -> t.outputs

    run startState |> drawScreen |> Map.find (-1L, 0L) 
 
input |> partOne |> printfn "%A"
input |> partTwo |> printfn "%A"