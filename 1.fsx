open System.IO

let input = File.ReadLines("input\\1.txt") |> Seq.map int
 
let fuelForMass mass = (mass / 3) - 2
 
let rec totalFuel mass =
    match fuelForMass mass with
    | m when m <= 0 -> 0
    | m -> m + (fuelForMass m)

let partOne = Seq.sumBy fuelForMass
let partTwo = Seq.sumBy totalFuel
 
input |> partOne |> printfn "%O"
input |> partTwo |> printfn "%O"