open System.IO

let input = File.ReadLines("input\\5.txt") |> Seq.head |> (fun x -> x.Split(',')) |> Array.map int

type ParameterMode =
    | Position of int
    | Immediate of int

let (|OpCode|) (values: int[]) =
    let code = values.[0] % 100
    let param n = ((values.[0] - code) % (pown 10 (n + 3))) / (pown 10 (n + 2))
    let mode n = values.[n + 1] |> if param n = 1 then Immediate else Position

    (code, mode 0, mode 1, mode 2)

let run input instructions =
    let rec runProgram (idx: int) (instructions: int []) =
        let get param =
            match param with
            | Immediate i -> i
            | Position p -> Array.get instructions p

        let doOp op x y out = 
            op (get x) (get y) |> Array.set instructions out
            runProgram (idx + 4) instructions

        let readInput x =
            input |> Array.set instructions x
            runProgram (idx + 2) instructions

        let output x =
            get x |> printfn "Output: %i"
            runProgram (idx + 2) instructions

        let jumpIf condition x y =
            if (get x) |> condition
            then runProgram (get y) instructions
            else runProgram (idx + 3) instructions

        let doCompare cmp x y out =
            if cmp (get x) (get y) then Array.set instructions out 1 else Array.set instructions out 0
            runProgram (idx + 4) instructions

        match instructions.[idx..idx+3] with
        | OpCode (99, _, _, _) -> instructions.[0]
        | OpCode (1, x, y, Position out) -> doOp (+) x y out
        | OpCode (2, x, y, Position out) -> doOp (*) x y out
        | OpCode (3, Position out, _, _) -> readInput out
        | OpCode (4, x, _, _) -> output x
        | OpCode (5, x, y, _) -> jumpIf ((<>) 0) x y
        | OpCode (6, x, y, _) -> jumpIf ((=) 0) x y
        | OpCode (7, x, y, Position out) -> doCompare (<) x y out
        | OpCode (8, x, y, Position out) -> doCompare (=) x y out
        | _ -> invalidArg "instructions" ("invalid op code: " + (string instructions.[idx]))

    runProgram 0 instructions

let partOne = run 1 (Array.copy input)
let partTwo = run 5 (Array.copy input)
 
partOne |> printfn "%O"
partTwo |> printfn "%O"