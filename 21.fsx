#load "common.fsx"
#load "intcode.fsx"

open System.IO
open Common
open IntCode
 
let input = File.ReadLines("input\\21.txt") |> Seq.head |> (fun x -> x.Split(',')) |> Array.map int64

let partOne input =
    // (!A || !B || !C ) && D
    let program =
        [
            "NOT A T\n";
            "NOT B J\n";
            "OR T J\n";
            "NOT C T\n";
            "OR T J\n";
            "AND D J\n";
            "WALK\n";
        ]
        |> List.collect ((Seq.map (char >> int64)) >> List.ofSeq)
    run program input

let partTwo input =
    // (!A || !B || !C) && D && (H || (E && (F || G || I)))
    let program =
        [
            "NOT A T\n";
            "NOT B J\n";
            "OR T J\n";
            "NOT C T\n";
            "OR T J\n";
            "OR T J\n";
            "AND D J\n";
            "NOT F T\n";
            "NOT T T\n";
            "OR G T\n";
            "OR I T\n";
            "AND E T\n";
            "OR H T\n";
            "AND T J\n";
            "RUN\n";
        ]
        |> List.collect ((Seq.map (char >> int64)) >> List.ofSeq)
    run program input

input |> partOne |> printfn "%A"
input |> partTwo |> printfn "%O"