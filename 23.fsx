#load "common.fsx"
#load "intcode.fsx"

open System.IO
open Common
open IntCode
 
let input = File.ReadLines("input\\23.txt") |> Seq.head |> (fun x -> x.Split(',')) |> Array.map int64

let stepAll = List.map (stepState >> getState)
let newPackets = List.collect (fun state -> state.outputs |> List.rev |> List.chunkBySize 3)
let isNetworkIdle = List.collect (fun c -> c.inputs @ c.outputs) >> List.isEmpty

let routePackets computers =
    let outputs = newPackets computers

    computers
    |> List.mapi (fun i state ->
        let inputs =
            outputs
            |> List.filter (fun o -> List.head o = int64 i)
            |> List.collect List.tail

        {state with outputs = []; inputs = if List.isEmpty inputs then [-1L] else inputs}
    )

let rec runNetwork nat last computers =
    let updated = computers |> routePackets |> stepAll 

    if isNetworkIdle updated
    then
        if last = None then printfn "%A" nat
        if nat = last
        then nat
        else
            List.mapi (fun i c ->
                if i = 0 then (stepState {c with inputs = nat |> Option.get |> List.tail} |> getState) else c
            ) updated
            |> runNetwork nat nat
    else
        let newNat =
            updated
            |> newPackets
            |> List.rev
            |> List.tryFind (fun p -> List.head p = 255L) 
        runNetwork (newNat |? nat) last updated
    
[0L..49L]
|> List.map (fun addr -> initState input [addr])
|> stepAll
|> runNetwork None None
|> printfn "%A"