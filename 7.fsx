#load "common.fsx"
#load "intcode.fsx"

open System.IO
open Common
open IntCode
 
let input = File.ReadLines("input\\7.txt") |> Seq.head |> (fun x -> x.Split(',')) |> Array.map int

let partOne () =
    permute [0; 1; 2; 3; 4]
    |> List.map (
        List.fold (fun x p ->
            run [p; x] input
            |> (fun state -> List.head state.outputs)
        ) 0)
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
        let states = Array.map (fun p -> Running {idx = 0; inputs = [p]; outputs = []; instructions = Array.copy input; relativeBase = 0}) (Array.ofList phases)
        Array.set states 0 (Running {getState states.[0] with inputs = List.append (getState states.[0]).inputs [0]})
        run states 0)
    |> List.max

let intcodeActor startState =

    MailboxProcessor.Start(fun inbox ->
        let mutable state = startState
        let rec loop () =
            async {
                let! (msg, channel: AsyncReplyChannel<int>) = inbox.Receive()
                state <- stepState {getState state with inputs = msg :: (getState state).inputs}
                state
                |> getState
                |> (fun s -> List.head s.outputs)
                |> channel.Reply
                return! loop ()
            }

        loop ()
    )


let goodPartTwo () =
    let actors =
        [5; 6; 7; 8; 9]
        |> List.map (fun p -> Running {idx = 0; inputs = [p]; outputs = []; instructions = Array.copy input; relativeBase = 0} |> intcodeActor)
        
    actors.[0].PostAndReply(fun rc -> 0, rc)
    
partOne () |> printfn "%A"
partTwo () |> printfn "%A"
