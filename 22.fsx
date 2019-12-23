#load "common.fsx"

open System.IO
open Common
 
let input = File.ReadLines("input\\22.txt") |> List.ofSeq

type Instruction =
    | Stack
    | Cut of bigint
    | Deal of bigint

let parse instruction =
    match instruction with
    | "deal into new stack" -> Stack
    | inst when inst.StartsWith("cut") -> inst.Split(' ').[1] |> bigint.Parse |> Cut
    | inst when inst.StartsWith("deal") -> inst.Split(' ').[3] |> bigint.Parse |> Deal
    | _ -> failwith instruction

let reverseShuffle instructions deck card =
    instructions
    |> List.rev
    |> List.fold (fun idx instruction ->
        match parse instruction with
        | Stack ->  deck - idx - 1I
        | Cut n -> (idx + n + deck) % deck
        | Deal n -> (inverseModP deck n * idx) % deck
    ) card

let partOne input =
    Array.find (fun c -> reverseShuffle input 10007I c = 2019I) [|0I..10006I|]

let partTwo input =
    let deck = 119315717514047I
    let shuffles = 101741582076661I
    let card = 2020I
    let invert = inverseModP deck

    // Let reverseShuffle instructions deck card = a.card + b
    // ac + b = x
    // ax + b = y
    let x = reverseShuffle input deck card
    let y = reverseShuffle input deck x

    // a = (x - y)/(c - x)
    // b = x - ac
    let a = ((x - y) * invert (card - x + deck) % deck ) + deck 
    let b = (x - (a * card) % deck) + deck

    // call reverseShuffle 101741582076661 times
    let an = expMod a shuffles deck
    an * card + b * (an - 1I) * invert (a - 1I) % deck
 
input |> partOne |> printfn "%A"
input |> partTwo |> printfn "%A"
