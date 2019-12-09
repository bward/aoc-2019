#load "common.fsx"

open System.IO
open Common
 
let input =
    File.ReadLines("input\\6.txt")
        |> Seq.map (fun s -> s.Split(')'))
        |> Seq.map (fun l -> (l.[0], l.[1]))

let partOne input =
    let bodies =
        let first = Seq.map fst input 
        let second = Seq.map snd input
        Set.ofSeq (Seq.append first second)
    
    let doCounts body counts =
        let n =
            input 
            |> Seq.where (fun (_, a) -> a = body)
            |> Seq.length
        let old =
            let orbiting = Seq.find (fun (_, x) -> x = body) input |> fst
            match Map.tryFind orbiting counts with
            | Some c -> c
            | None -> 0
        n + old

    let rec run bodies counts =
        let addBody counts body =
            Map.add body (doCounts body counts) counts
        let neighbours =
            Seq.where (fun (a, _) -> List.contains a bodies) input
            |> Seq.map snd
            |> List.ofSeq

        match bodies with
        | [] -> counts
        | _ -> 
            List.fold addBody counts bodies
            |> run neighbours 
        
    run ["46J"] Map.empty
    |> Map.toSeq
    |> Seq.sumBy snd


let partTwo input =
    let rec pathToCom body =
        match Seq.find (fun (_, x) -> x = body) input |> fst with
        | "COM" -> ["COM"]
        | other -> other :: pathToCom other
    
    let rec pathToYou path body =
        match Seq.find (fun (_, x) -> x = body) input |> fst with
        | x when (Set.contains x path) -> ([x], x)
        | other ->
            let rest = pathToYou path other
            (other :: fst rest, snd rest)
    
    let youToCom = pathToCom "YOU"
    let sanToYou = pathToYou (Set.ofList youToCom) "SAN"
    List.length (fst sanToYou) + List.findIndex ((=) (snd sanToYou)) youToCom - 1

 
input |> partOne |> printfn "%A"
input |> partTwo |> printfn "%A"