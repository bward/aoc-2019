open System.IO
open System.Collections.Generic
 
let input =
    File.ReadLines("input\\20.txt") 
    |> Seq.map (Seq.map string)
    |> Seq.mapi (fun y row ->
        Seq.mapi (fun x ch ->
            ((x, y), ch)
        ) row
    )
    |> Seq.collect id
    |> Map.ofSeq

let isKeyChar c = "A" <= c && c <= "Z"

let width = input |> Map.toList |> List.maxBy (fun ((x, _), _) -> x) |> (fun ((x, _), _) -> x)
let height = input |> Map.toList |> List.maxBy (fun ((_, y), _) -> y) |> (fun ((_, y), _) -> y)
let isInterior (x, y) = x > 3 && x < (width - 3) && y > 3 && y < (height - 3)

let findPortals map =
    map
    |> Map.toList
    |> List.choose (fun ((x, y), v) -> 
        if isKeyChar v
        then
            match [(x+1, y); (x, y+1)] |> List.map (fun p -> Map.tryFind p map) with
            | [Some c; _] when isKeyChar c ->
                if Map.tryFind (x-1, y) map = Some "."
                then Some (v+c, (x, y), (x-1, y))
                else Some (v+c, (x+1, y), (x+2, y))
            | [_; Some c] when isKeyChar c ->
                if Map.tryFind (x, y-1) map = Some "."
                then Some (v+c, (x, y), (x, y-1))
                else Some (v+c, (x, y+1), (x, y+2))
            | _ -> None
        else None
    )
    |> Seq.groupBy (fun (portal, _, _) -> portal)
    |> Map.ofSeq
    |> Map.map (fun _ vals-> Seq.map (fun (_, x, y) -> (x, y)) vals)

let solveMaze depthChange input =
    let portals = findPortals input
    let start = Map.find "AA" portals |> Seq.item 0 |> snd
    let goal = Map.find "ZZ" portals |> Seq.item 0 |> snd

    let queue = new Queue<((int*int)*int)list*int>()
    let seen = new HashSet<(int*int)*int>()

    let rec bfs () = 
        let path, dist = queue.Dequeue()
        let ((x, y), depth) = List.head path

        if ((x, y), depth) = (goal, 0)
        then dist
        else
            if seen.Contains(((x, y), depth)) || depth < 0
            then bfs ()
            else
                seen.Add(((x, y), depth)) |> ignore
                if seen.Count % 10000 = 0 then printfn "%A" seen.Count
                let neighbours =
                    [(x-1, y); (x+1, y); (x, y-1); (x, y+1)]
                    |> List.choose (
                        (fun pos -> (pos, Map.find pos input))
                        >> (fun (pos, ch) -> 
                            let portal = Map.tryFindKey (fun k v -> Seq.exists (fun (entrance, _) -> entrance = pos) v) portals
                            match portal with
                            | Some p when p <> "AA" && p <> "ZZ" ->
                                let gates = Map.find p portals
                                let exit = Seq.find (fun (entrance, _) -> entrance <> pos) gates |> snd
                                Some (exit, ".", depthChange pos depth)
                            | _ -> Some (pos, ch, depth)
                        )
                    )
                    |> List.where (fun (pos, ch, depth) -> ch = "." && seen.Contains((pos, depth)) |> not)

                List.iter (fun (pos, _, depth) -> queue.Enqueue(((pos, depth) :: path, dist + 1))) neighbours |> ignore
                bfs ()
    queue.Enqueue(([(start, 0)], 0))
    bfs ()

input |> solveMaze (fun _ _ -> 0) |> printfn "%A"
input |> solveMaze (fun pos depth -> if isInterior pos then depth + 1 else depth - 1) |> printfn "%A"