#load "common.fsx"

open System.IO
 
let input = File.ReadLines("input\\16.txt") |> Seq.head |> Seq.map (string >> int) |> Array.ofSeq
let length = Seq.length input

let baseArr = [| 0; 1; 0; -1|]
let getCoeff n i =
    let period = 4 * n
    let pos = (i % period) / n
    baseArr.[pos]

let partOne input =
    let iterate input =
        Array.init length (fun n -> Array.map2 (*) input (Array.map (fun i -> getCoeff (n+1) i) [|1..length|]) |> Array.sum)
            |> Array.map (fun x -> (abs x) % 10)
    input |> List.fold (>>) id (List.replicate 100 iterate) |> (fun arr -> arr.[0..7]) |> Array.map string |> String.concat ""

let partTwo (input: int array) =
    let goal = input.[0..6] |> Array.map string |> String.concat "" |> int
    let repeatedInput = seq { for _ in [1..10000] do yield! input} |> Seq.skip goal |> Array.ofSeq
    let iterate (x: int array) =
        let reverse = Array.rev x
        Array.fold (fun sums y -> (y + (if List.isEmpty sums then 0 else List.head sums)) % 10 :: sums) [] reverse |> Array.ofList
    
    repeatedInput |> List.fold (>>) id (List.replicate 100 iterate) |> (fun arr -> arr.[0..7]) |> Array.map string |> String.concat ""
    
input |> partOne |> printfn "%A"
input |> partTwo |> printfn "%A"