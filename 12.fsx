#load "common.fsx"

open System.IO
open Common

type Moon = {Pos: int * int * int; Vel: int * int * int}
 
let input = 
    [
        {Pos = (3, -6, 6); Vel = (0, 0, 0)};
        {Pos = (10, 7, -9); Vel = (0, 0, 0)};
        {Pos = (-3, -7, 9); Vel = (0, 0, 0)};
        {Pos = (-8, 0, 4); Vel = (0, 0, 0)}
    ]

let sum3 (a, b, c) (d, e, f) = (a + d, b + e, c + f)

let delta x1 x2 =
        match x1 - x2 with
        | 0 -> 0
        | x when x > 0 -> -1
        | x when x < 0 -> 1
        | _ -> invalidArg "wat" "wat"

let rec applyGravity moons =
    let updateMoon moons moon=
        let newVel = 
            match moon with
            | {Pos = (p1, p2, p3)} -> List.fold (fun vel {Pos = (m1, m2, m3)} -> (delta p1 m1, delta p2 m2, delta p3 m3) |> sum3 vel) moon.Vel moons
        {moon with Vel = newVel}

    List.map (updateMoon moons) moons

let applyVelocity moons =
    List.map (fun m -> {m with Pos = sum3 m.Pos m.Vel}) moons

let energy moons =
    List.sumBy (fun {Pos = (p1, p2, p3); Vel = (v1, v2, v3)} -> ((abs p1) + (abs p2) + (abs p3))*((abs v1) + (abs v2) + (abs v3))) moons

let rec applyGravity1d moons =
    let updateMoon moons (p, v) =
        (p, List.fold (fun v (p2, _) -> v + (delta p p2)) v moons)
    List.map (updateMoon moons) moons

let applyVelocity1d moons =
    List.map (fun (p, v) -> (p+v, v)) moons

let rec findPeriod1d start current elapsed =
    let newMoons = current |> applyGravity1d |> applyVelocity1d
    if start = newMoons then (elapsed + 1) else findPeriod1d start newMoons (elapsed + 1)

let partOne input = List.fold (fun moons _ -> applyGravity moons |> applyVelocity) input [1..1000] |> energy

let partTwo input =
    let moons = [(6, 0); (-9, 0); (9, 0); (4, 0)]
    findPeriod1d moons moons 0
    //List.fold (fun moons _ -> applyGravity1d moons |> applyVelocity1d) moons [1..2772]

 
//input |> partOne |> printfn "%A"
input |> partTwo |> printfn "%A"