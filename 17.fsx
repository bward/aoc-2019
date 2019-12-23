#load "intcode.fsx"

open System.IO
open IntCode
 
let input = File.ReadLines("input\\17.txt") |> Seq.head |> (fun x -> x.Split(',')) |> Array.map int64

let parseOutput output =
    output
    |> List.rev
    |> List.map (char >> string)
    |> (fun l -> List.chunkBySize ((List.findIndex (fun x -> x = "\n") l) + 1) l)
    |> List.mapi (fun y row ->
        List.mapi (fun x value ->
            ((x, y), value)
        ) row
    )
    |> List.collect id
    |> Map.ofList

let partOne input =
    (run [] input).outputs
    |> parseOutput
    |> (fun world ->
        Map.filter (fun (x, y) value ->
            if value = "#"
            then 
                [(x, y-1); (x, y+1); (x-1, y); (x+1, y)]
                    |> List.map (fun coords -> Map.tryFind coords world)
                    |> List.forall ((=) (Some "#"))
            else false
        ) world
    )
    |> Map.toList
    |> List.sumBy (fun ((x, y), _) -> x * y)

let partTwo input =
    let mainRoutine = "A,A,B,C,B,A,C,B,C,A\n" 
    let a = "L,6,R,12,L,6,L,8,L,8\n"
    let b = "L,6,R,12,R,8,L,8\n"
    let c = "L,4,L,4,L,6\n"
    let video = "n\n"

    let inputs = String.concat "" [mainRoutine; a; b; c; video] |> Seq.map int64 |> List.ofSeq
    Array.set input 0 2L 

    (run inputs input).outputs |> List.head

input |> partOne |> printfn "%O"
input |> partTwo |> printfn "%A"