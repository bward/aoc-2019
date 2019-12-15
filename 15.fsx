#load "common.fsx"
#load "intcode.fsx"

open System.IO
open Common
open IntCode
 
let input = File.ReadLines("input\\15.txt") |> Seq.head |> (fun x -> x.Split(',')) |> Array.map int64

let commands = [(1L, (0, 1)); (2L, (0, -1)); (3L, (-1, 0)); (4L, (1, 0))]

let getNeighbours state pos seen =
    commands
    |> List.choose (fun (cmd, dir) ->
        let newState = stepState {state with inputs = [cmd]; instructions = Array.copy state.instructions} |> getState
        match List.head newState.outputs with
        | 0L -> None
        | _ ->
            let newPos = add2 dir pos
            if Set.contains newPos seen then None else Some (cmd, newPos, newState)
    )

let partOne input =
    let rec run seen q =
        let path, newQ = Queue.dequeue q
        let (_, pos, state) = List.head path

        match state.outputs with
        | 2L :: _ -> (List.length path - 1, List.head path |> (fun (_, x, _) -> x), List.map (fun (x, _, _) -> x) path |> List.rev)
        | _ ->
            getNeighbours state pos seen
                |> List.fold (fun acc x -> Queue.enqueue (x :: path) acc) newQ
                |> run (seen |> Set.add pos)

    let startState = {idx = 0; instructions = Array.concat  [(Array.copy input); (Array.create 1000 0L)]; inputs = []; outputs = []; relativeBase = 0}
    run (Set.empty |> Set.add (0, 0)) (Queue.empty|> Queue.enqueue [(1L, (0, 0), startState)]) 

let partTwo input source steps =
    let rec run seen steps =
        let neighbours =
            List.head steps 
            |> List.collect (fun (pos, state) ->
                getNeighbours state pos seen
            )
            |> List.map (fun (_, p, s) -> (p, s))

        match neighbours with
        | [] -> List.length steps - 1
        | _ ->
            let newSeen = List.fold (fun acc (pos, _) -> Set.add pos acc) seen neighbours 
            run newSeen (neighbours :: steps)

    let startState = stepState {idx = 0; instructions = Array.concat  [(Array.copy input); (Array.create 1000 0L)]; inputs = steps; outputs = []; relativeBase = 0} |> getState
    run (Set.empty |> Set.add source) [[(source, startState)]]

 
let (distance, source, steps) = input |> partOne
printfn "%A" distance
partTwo input source steps |> printfn "%A"