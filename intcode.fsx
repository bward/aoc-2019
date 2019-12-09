module IntCode

type ParameterMode =
    | Position of bigint
    | Immediate of bigint
    | Relative of bigint

type State = {idx: int; instructions: bigint[]; inputs: int list; outputs: bigint list; relativeBase: int}

type RunMode =
    | Running of State
    | Terminated of State

let (|OpCode|) (values: bigint[]) =
    let code = values.[0] % (bigint 100)
    let param n = ((values.[0] - code) % (pown (bigint 10) (n + 3))) / (pown (bigint 10) (n + 2))
    let mode n =
        values.[n + 1]
        |> match param n with
            | c when c = bigint 0 -> Position
            | c when c = bigint 1 -> Immediate
            | c when c = bigint 2 -> Relative
            | _ -> invalidArg "paramMode" "invalid paramter mode"

    (code, mode 0, mode 1, mode 2)

let rec stepState (state: State) =
    let get param =
        match param with
        | Immediate i -> i
        | Position p -> Array.get state.instructions (int p)
        | Relative r -> Array.get state.instructions (state.relativeBase + (int r))
    
    let getOut param =
        match param with
        | Immediate _ -> invalidArg "param" "can't output to immediate"
        | Position p -> int p
        | Relative r -> state.relativeBase + (int r)

    let doOp op x y out = 
        op (get x) (get y) |> Array.set state.instructions (getOut out)
        stepState {state with idx = state.idx + 4}

    let readInput x =
        match state.inputs with
        | i :: is ->
            Array.set state.instructions (getOut x) (bigint i)
            stepState {state with idx = state.idx + 2; inputs = is}
        | [] -> invalidArg "inputs" "out of input values"

    let output x =
        Running {state with idx = state.idx + 2; outputs = get x :: state.outputs}

    let jumpIf condition x y =
        if (get x) |> condition
        then stepState {state with idx = get y |> int}
        else stepState {state with idx = state.idx + 3}

    let doCompare cmp x y out =
        if cmp (get x) (get y) then Array.set state.instructions (getOut out) (bigint 1) else Array.set state.instructions (getOut out) (bigint 0)
        stepState {state with idx = state.idx + 4}
    
    let adjustRelativeBase x =
        stepState {state with idx = state.idx + 2; relativeBase = state.relativeBase + (get x |> int)}

    match state.instructions.[state.idx..state.idx+3] with
    | OpCode (c, _, _, _) when c = bigint 99 -> Terminated state
    | OpCode (c, x, y, out) when c = bigint 1 -> doOp (+) x y out
    | OpCode (c, x, y, out) when c = bigint 2 -> doOp (*) x y out
    | OpCode (c, out, _, _) when c = bigint 3 -> readInput out
    | OpCode (c, x, _, _) when c = bigint 4 -> output x
    | OpCode (c, x, y, _) when c = bigint 5 -> jumpIf ((<>) (bigint 0)) x y
    | OpCode (c, x, y, _) when c = bigint 6 -> jumpIf ((=) (bigint 0)) x y
    | OpCode (c, x, y, out) when c = bigint 7 -> doCompare (<) x y out
    | OpCode (c, x, y, out) when c = bigint 8 -> doCompare (=) x y out
    | OpCode (c, x, _, _) when c = bigint 9 -> adjustRelativeBase x
    | _ -> invalidArg "instructions" ("invalid op code: " + (string state.instructions.[state.idx]))

let rec runState (state: State) = 
    match stepState state with
    | Running s -> runState s
    | Terminated s -> s

let run (inputs: int list) (instructions: bigint[]) =
    runState {idx = 0; inputs = inputs; outputs = []; instructions = Array.append (Array.copy instructions) (Array.create 1000 (bigint 0)); relativeBase = 0}

let terminated runState =
    match runState with
    | Running _ -> false
    | Terminated _ -> true

let getState runState =
    match runState with
    | Running s -> s
    | Terminated s -> s

