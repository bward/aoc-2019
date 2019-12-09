module IntCode

type ParameterMode =
    | Position of int64
    | Immediate of int64
    | Relative of int64

type State = {idx: int; instructions: int64[]; inputs: int64 list; outputs: int64 list; relativeBase: int}

type RunMode =
    | Running of State
    | Terminated of State

let (|OpCode|) (values: int64[]) =
    let code = (int values.[0]) % 100
    let param n = ((int values.[0] - code) % (pown 10 (n + 3))) / (pown 10 (n + 2))
    let mode n =
        values.[n + 1]
        |> match param n with
            | 0 -> Position
            | 1 -> Immediate
            | 2 -> Relative
            | _ -> invalidArg "paramMode" "invalid paramter mode"

    (code, mode 0, mode 1, mode 2)

let rec stepState (state: State) =
    let get param =
        match param with
        | Immediate i -> i
        | Position p -> int p |> Array.get state.instructions
        | Relative r ->  state.relativeBase + int r |> Array.get state.instructions
    
    let getOut param =
        match param with
        | Immediate _ -> invalidArg "param" "can't output to immediate"
        | Position p -> int p
        | Relative r -> state.relativeBase + int r

    let doOp op x y out = 
        op (get x) (get y) |> Array.set state.instructions (getOut out)
        stepState {state with idx = state.idx + 4}

    let readInput x =
        match state.inputs with
        | i :: is ->
            Array.set state.instructions (getOut x) (int64 i)
            stepState {state with idx = state.idx + 2; inputs = is}
        | [] -> invalidArg "inputs" "out of input values"

    let output x =
        Running {state with idx = state.idx + 2; outputs = get x :: state.outputs}

    let jumpIf condition x y =
        if (get x) |> condition
        then stepState {state with idx = get y |> int}
        else stepState {state with idx = state.idx + 3}

    let doCompare cmp x y out =
        if cmp (get x) (get y) then Array.set state.instructions (getOut out) 1L else Array.set state.instructions (getOut out) 0L
        stepState {state with idx = state.idx + 4}
    
    let adjustRelativeBase x =
        stepState {state with idx = state.idx + 2; relativeBase = state.relativeBase + (get x |> int)}

    match state.instructions.[state.idx..state.idx+3] with
    | OpCode (99, _, _, _) -> Terminated state
    | OpCode (1, x, y, out) -> doOp (+) x y out
    | OpCode (2, x, y, out) -> doOp (*) x y out
    | OpCode (3, out, _, _) -> readInput out
    | OpCode (4, x, _, _) -> output x
    | OpCode (5, x, y, _) -> jumpIf ((<>) 0L) x y
    | OpCode (6, x, y, _) -> jumpIf ((=) 0L) x y
    | OpCode (7, x, y, out) -> doCompare (<) x y out
    | OpCode (8, x, y, out) -> doCompare (=) x y out
    | OpCode (9, x, _, _) -> adjustRelativeBase x
    | _ -> invalidArg "instructions" ("invalid op code: " + (string state.instructions.[state.idx]))

let rec runState (state: State) = 
    match stepState state with
    | Running s -> runState s
    | Terminated s -> s

let run (inputs: int64 list) (instructions: int64[]) =
    runState {idx = 0; inputs = inputs; outputs = []; instructions = Array.append (Array.copy instructions) (Array.create 1000 0L); relativeBase = 0}

let terminated runState =
    match runState with
    | Running _ -> false
    | Terminated _ -> true

let getState runState =
    match runState with
    | Running s -> s
    | Terminated s -> s

