#load "common.fsx"

open System.IO
open Common
 
let parsePair (pair: array<string>) =
    (pair.[1], int64 pair.[0])

let input =
    File.ReadLines("input\\14.txt")
    |> Seq.map (
        (fun line -> line.Split([|" => "|], System.StringSplitOptions.None))
        >> (fun line ->
            let resultParts = line.[1].Split(' ')
            let result = parsePair resultParts
            let components = line.[0] |> (fun c -> c.Split([|", "|], System.StringSplitOptions.None)) |> Array.map ((fun c -> c.Split(' ')) >> parsePair)
            (fst result, (snd result, List.ofArray components))
        )
    )
    |> Map.ofSeq

let addChem c n m =
    match Map.tryFind c m with
    | Some existing -> Map.add c (n + existing) m
    | None -> Map.add c n m

let oreForFuel fuel =
    let rec react chemicals =
        match Map.toList chemicals with
        | reactants when List.tryFind (fun (c, n) -> c <> "ORE" && n > 0L) reactants = None -> Map.find "ORE" chemicals
        | reactants ->
            List.fold (fun products reactant -> 
                match reactant with
                | (chemical, n) when chemical = "ORE" || n <= 0L -> addChem chemical n products
                | (chemical, n) -> 
                    let (required, chems) = Map.find chemical input 
                    let needed = (n + required - 1L) / required
                    List.fold (fun state (c, n) -> addChem c (n * needed) state) products chems
                    |> addChem chemical (n - required * needed)
            ) Map.empty reactants
            |> react
    
    Map.empty |> Map.add "FUEL" fuel |> react

let partOne = oreForFuel 1L
let partTwo = (binarySearch (fun x -> oreForFuel x > 1000000000000L) 0L 100000000L) - 1L

partOne |> printfn "%A"
partTwo |> printfn "%A"