#load "common.fsx"

open System.IO
open System
open Common
 
type Move = 
    | Up of int
    | Down of int
    | Left of int
    | Right of int

let input =
    File.ReadLines("input\\3.txt")
        |> Seq.map (fun x ->
            x.Split(',')
                |> List.ofArray
                |> List.map (fun move -> 
                    match Seq.head move with
                        | 'U' -> Up (Seq.tail move |> String.Concat |> int)
                        | 'D' -> Down (Seq.tail move |> String.Concat |> int)
                        | 'L' -> Left (Seq.tail move |> String.Concat |> int)
                        | 'R' -> Right (Seq.tail move |> String.Concat |> int)
                        | _ -> invalidArg "input" "unrecognised direction"
                )
        )

let points directions =
    let move points dir =
        match List.head points with
            (x, y), total ->
                match dir with
                    | Up d -> List.append (List.map (fun t -> ((x, y+t), total + t)) [d..(-1)..1]) points
                    | Down d -> List.append (List.map (fun t -> ((x, y+t), total + abs t)) [-d..(-1)]) points
                    | Left d -> List.append (List.map (fun t -> ((x+t, y), total + abs t)) [-d..(-1)]) points
                    | Right d -> List.append (List.map (fun t -> ((x+t, y), total + t)) [d..(-1)..1]) points

    List.fold move [((0, 0), 0)] directions

let partOne input =
    let [first; second] =
        Seq.map points input
        |> List.ofSeq
    printfn "%O" first
    let common =
        let f = Set.map fst (Set.ofList first)
        let s = Set.map fst (Set.ofList second)
        Set.intersect f s
        |> Set.remove (0, 0)
    Set.map (fun (x, y) -> abs x + abs y) common
    |> Set.minElement

let partTwo input =
    let [first; second] =
        Seq.map points input
        |> List.ofSeq
    printfn "%O" first
    let common =
        let f = Set.map fst (Set.ofList first)
        let s = Set.map fst (Set.ofList second)
        Set.intersect f s
        |> Set.remove (0, 0)
    Set.map (fun p -> snd (List.find (fun (pt, _) -> pt = p) first) + snd (List.find (fun (pt, _) -> pt = p) second)) common
    |> Set.minElement

 
input |> partOne |> printfn "%A"
input |> partTwo |> printfn "%O"