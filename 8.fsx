open System.IO

let input =
    File.ReadLines("input\\8.txt")
    |> Seq.head 
    |> Seq.map (string >> int)
    |> List.ofSeq
    |> List.chunkBySize 25
    |> List.chunkBySize 6

let getPixel input x y =
    input
    |> List.map (List.item y >> List.item x)
    |> List.find ((<>) 2)

let partOne input =
    input
    |> List.minBy (List.collect id >> List.where ((=) 0) >> List.length)
    |> List.collect id
    |> List.countBy id

let partTwo input =
    [for y in 0..5 do yield [for x in 0..24 do yield getPixel input x y]]
 
input |> partOne |> printfn "%A"
input |> partTwo |> printfn "%A"