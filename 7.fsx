#load "common.fsx"
#load "intcode.fsx"

open System.IO
open Common
open IntCode
 
let input = File.ReadLines("input\\7.txt") |> Seq.head |> (fun x -> x.Split(',')) |> Array.map int64

let partOne () =
    permute [0; 1; 2; 3; 4]
    |> List.map (
        List.fold (fun x p ->
            run [int64 p; x] input
            |> (fun state -> List.head state.outputs)
        ) 0L)
    |> List.max

let partTwo () =
    let rec run states idx =
        match states with
        | [|_; _; _; _; s|] when Array.exists terminated states -> List.head (getState s).outputs
        | _ ->
            let newState = states.[idx] |> getState |> stepState
            let nextIdx = (idx + 1) % 5
            let nextState = states.[nextIdx] |> getState

            Array.set states idx newState
            Array.set states nextIdx (Running {nextState with inputs = List.append nextState.inputs [getState newState |> (fun s -> List.head s.outputs)]})

            run states nextIdx

    permute [5; 6; 7; 8; 9]
    |> List.map (fun phases -> 
        let states = Array.map (fun p -> Running {idx = 0; inputs = [int64 p]; outputs = []; instructions = Array.copy input; relativeBase = 0}) (Array.ofList phases)
        Array.set states 0 (Running {getState states.[0] with inputs = List.append (getState states.[0]).inputs [0L]})
        run states 0)
    |> List.max


type MyType = 
    | Run of int64
    | Done

let intcodeActor startState =
    MailboxProcessor.Start(fun inbox ->
        let rec loop state =
            async {
                let! (msg, channel: AsyncReplyChannel<MyType>) = inbox.Receive()
                let newState = stepState {getState state with inputs = i :: (getState state).inputs}
                match msg with
                | Run i -> 
                    match newState with
                    | Running s -> 
                        List.head s.outputs
                        |> Run
                        |> channel.Reply
                    | Terminated _ -> channel.Reply Done
                | Done -> ()

                return! loop newState
            }

        loop startState
    )

let goodPartTwo () =
    let actors =
        [5L; 6L; 7L; 8L; 9L]
        |> List.map (fun p -> Running {idx = 0; inputs = [p]; outputs = []; instructions = Array.copy input; relativeBase = 0} |> intcodeActor)
        |> Seq.ofList
        
    Seq.scan (fun (s: MyType) (a: MailboxProcessor<MyType * AsyncReplyChannel<MyType>>) ->
        a.PostAndReply(fun rc -> s, rc)
    ) (Run 0L) (cycle actors)
    
partOne () |> printfn "%A"
partTwo () |> printfn "%A"
goodPartTwo () |> printfn "%A"
