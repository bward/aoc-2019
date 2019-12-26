#load "common.fsx"

open System.IO
open Common

let input = 
    File.ReadLines("input\\24.txt")
    |> Seq.mapi (fun y row ->
        row |> Seq.mapi (fun x v -> ((0, y, x), string v))
    )
    |> Seq.collect id
    |> Map.ofSeq

let depth ((d, _, _), _) = d

let step getNeighbours world =
    world
    |> Map.map (fun (d, x, y) v ->
        let neighbours = 
            (d, x, y)
            |> getNeighbours
            |> List.map (fun pos -> Map.tryFind pos world)
            |> List.filter ((=) (Some "#"))
            |> List.length

        if v = "#"
        then
            if neighbours = 1 then "#" else "."
        else
            if neighbours = 1 || neighbours = 2 then "#" else "."
    )

let biodiversity world =
    Map.toList world
    |> List.sumBy (fun ((_, y, x), v) ->
        if v = "."
        then 0
        else pown 2 (x + 5 * y)
    )

let expand world =
    let bugs =
        world
        |> Map.filter (fun _ v -> v = "#")
        |> Map.toList
    let minLevel = bugs |> List.map depth |> List.min
    let maxLevel = bugs |> List.map depth |> List.max

    List.fold (fun acc (x, y) ->
        acc
        |> Map.add (minLevel-1, y, x) "."
        |> Map.add (maxLevel+1, y, x) "."
    ) world [for x in 0..4 do yield! [for y in 0..4 do yield (x, y)]]

let neighboursInLevel (d, x, y) = [(d, x-1, y); (d, x+1, y); (d, x, y-1); (d, x, y+1)]

let recursiveNeighbours (d, y, x) = 
        if (y, x) = (2, 2)
        then []
        else
            neighboursInLevel (d, y, x)
            |> addIf (y = 0) [(d-1, 1, 2)]
            |> addIf (y = 4) [(d-1, 3, 2)]
            |> addIf (x = 0) [(d-1, 2, 1)]
            |> addIf (x = 4) [(d-1, 2, 3)]
            |> addIf (x = 2 && y = 1) [for x in 0..4 do yield (d+1, 0, x)]
            |> addIf (x = 2 && y = 3) [for x in 0..4 do yield (d+1, 4, x)]
            |> addIf (x = 1 && y = 2) [for y in 0..4 do yield (d+1, y, 0)]
            |> addIf (x = 3 && y = 2) [for y in 0..4 do yield (d+1, y, 4)]

let rec partOne seen world = 
    if Set.contains world seen
    then world |> biodiversity
    else partOne (seen |> Set.add world) (world |> step neighboursInLevel)
    

let partTwo input =
    [1..200]
    |> List.fold (fun world _ -> 
        world |> expand |> step recursiveNeighbours
    ) input
    |> Map.filter (fun _ v -> v = "#")
    |> Map.count
 
input |> partOne Set.empty |> printfn "%A"
input |> partTwo |> printfn "%A"