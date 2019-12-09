let input =
    seq {123257..647015}
        |> Seq.map (string >> (Seq.map (fun s -> int s - int '0')))

let increasing ints =
    Seq.pairwise ints
        |> Seq.forall (fun (x, y) -> x <= y)

let partOne input = 
    let hasPair ints =
        Seq.pairwise ints
        |> Seq.exists (fun (x, y) -> x = y)

    input
        |> Seq.where increasing
        |> Seq.where hasPair
        |> Seq.length


let partTwo input =
    let hasLonePair ints =
        let addToGroup groups x =
            match groups with
            | g :: gs when (List.contains x g) -> ((x :: g) :: gs)
            | _ -> [x] :: groups

        Seq.fold addToGroup [] ints
            |> List.exists (fun l -> List.length l = 2)

    input
        |> Seq.where increasing
        |> Seq.where hasLonePair
        |> Seq.length
 
input |> partOne |> printfn "%O"
input |> partTwo |> printfn "%O"