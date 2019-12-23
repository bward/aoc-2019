#load "common.fsx"

open System.IO
open System.Collections.Generic
 
let input = File.ReadLines("input\\test.txt") |> Seq.map (Seq.map string)
let isKey (k: string) = k <> k.ToUpper()
let isDoor (k: string) = k <> k.ToLower()

let keyCount map =
    map
    |> Map.filter (fun k (v: string) -> v <> v.ToUpper())
    |> Map.toList
    |> List.length

let reachableCache = new Dictionary<(int*int)*Set<string>, Set<string*(int*int)>>()
let reachableKeys map keys start = 
    let rec recurse boundary seen =
        let neighbours = 
            boundary
            |> List.collect (fun (x, y) -> [(x-1,y); (x+1, y); (x,y-1); (x,y+1)])
            |> List.where (fun pos -> Set.contains pos seen |> not)
            |> List.choose (fun pos ->
                Map.find pos map
                |> (fun v -> if v <> "#" && (not (isDoor v) || Set.contains (v.ToLower()) keys) then Some pos else None)
            )

        if List.isEmpty neighbours
        then seen |> Set.map (fun pos -> (Map.find pos map, pos)) |> Set.filter (fst >> isKey) |> Set.filter (fst >> (fun k -> Set.contains k keys |> not))
        else recurse neighbours (Set.union seen (neighbours |> Set.ofList))

    match reachableCache.TryGetValue((start, keys)) with
    | true, x -> 
        x
    | _ -> 
        let result = recurse [start] (Set.empty |> Set.add start)
        reachableCache.Add((start, keys), result)
        result

let distanceCache = new Dictionary<(int*int)*string*Set<string>, int>()

let pathToKey map startPos keys endKey =
    match distanceCache.TryGetValue((startPos, endKey, keys)) with
    | true, x ->
        x
    | _ -> 
        let queue = new Queue<(int*int) list>()
        let seen = new HashSet<int*int>()
        let rec recurse () =
            let path = queue.Dequeue()
            let (x, y) = List.head path
            if Map.find (x, y) map = endKey
            then List.length path - 1
            else
                seen.Add((x, y)) |> ignore
                let neighbours = 
                    [(x-1,y); (x+1, y); (x,y-1); (x,y+1)]
                    |> List.where (seen.Contains >> not)
                    |> List.choose (fun pos ->
                        Map.find pos map
                        |> (fun v -> if v <> "#" && (not (isDoor v) || Set.contains (v.ToLower()) keys) then Some pos else None)
                    )
                let newNewQueue =
                    List.iter (fun pos -> queue.Enqueue((pos :: path))) neighbours
                recurse ()
        queue.Enqueue([startPos])

        let result = recurse ()
        distanceCache.Add((startPos, endKey, keys), result)
        result

let parse input = 
    input
        |> Seq.mapi (fun y row -> 
            Seq.mapi (fun x value ->
                ((x, y), value)
            ) row
        ) 
        |> Seq.collect id
        |> Map.ofSeq

let partOne input =
    let map = parse input
    let start = Map.findKey (fun _ v -> v = "@") map
    let cache = new Dictionary<(int*int)*Set<string>, int>()
    let mutable count = 0

    let rec distance pos keys =
        count <- count + 1
        if count % 1000 = 0 then printfn "%A" count
        let reachable = reachableKeys map keys pos
        match cache.TryGetValue((pos, keys)) with
        | true, x -> x
        | _ -> 
            let remaining =
                if Set.isEmpty reachable
                then 0
                else
                    reachable
                    |> Set.map (fun (k, kPos) -> 
                        let d = (pathToKey map pos keys k) + (distance kPos (Set.add k keys))
                        if Set.count keys = 0 then printfn "%A" d
                        d
                    )
                    |> Set.minElement
            cache.Add((pos, keys), remaining)
            remaining

    distance start Set.empty

let partTwo input = ""
 
input |> partOne |> printfn "%A"
input |> partTwo |> printfn "%O"