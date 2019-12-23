#load "common.fsx"

open System.IO
open System.Collections.Generic
 
let input = File.ReadLines("input\\18.txt") |> Seq.map (Seq.map string)
let isKey (k: string) = k <> k.ToUpper()
let isDoor (k: string) = k <> k.ToLower()

let keyCount map =
    map
    |> Map.filter (fun k (v: string) -> v <> v.ToUpper())
    |> Map.toList
    |> List.length

let allKeys map =
    map
    |> Map.filter (fun _ (v: string) -> v <> v.ToUpper())
    |> Map.toList
    |> List.map snd
    |> Set.ofList

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
    let goal = keyCount map
    let all = allKeys map
    let queue = new Queue<(int*int)*Set<string>*int>()
    let seen = new HashSet<int*int*Set<string>>()

    let rec bfs () =
        let ((x, y), keys, dist) = queue.Dequeue()
        let state = (x, y, keys)
        if seen.Contains(state)
        then bfs ()
        else
            seen.Add(state) |> ignore
            if seen.Count % 100000 = 0 then printfn "%A" (seen.Count)

            match Map.tryFind (x, y) map with
            | None | Some "#" -> bfs ()
            | Some ch when "A" <= ch && ch <= "Z" && not (Set.contains (ch.ToLower()) keys) && Set.contains (ch.ToLower()) all -> bfs ()
            | Some ch -> 
                let newKeys = if "a" <=  ch && ch <= "z" then Set.add ch keys else keys
                if Set.count newKeys = goal
                then dist
                else 
                    [(x, y-1); (x+1, y); (x, y+1); (x-1, y)]
                    |> List.iter (fun newPos -> queue.Enqueue((newPos, newKeys, dist+1)))
                    bfs ()

    queue.Enqueue((start, Set.empty, 0))
    bfs ()

// QQ a non-silly part 2...
 
input |> partOne |> printfn "%A"