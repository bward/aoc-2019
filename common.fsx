let pi = System.Math.PI

let (<*>) xs ys = [for x in xs do for y in ys -> (x, y)]

let manhattan (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

let rec distribute e = function
    | [] -> [[e]]
    | x::xs' as xs -> (e::xs)::[for xs in distribute e xs' -> x::xs]

let rec permute = function
    | [] -> [[]]
    | e::xs -> List.collect (distribute e) (permute xs)

let rec cycle xs =
    seq {
      yield! xs
      yield! cycle xs
    }

let (./.) x y = 
    (x |> double) / (y |> double)

let rec binarySearch pred lower upper =
    if upper = lower 
    then upper
    else 
        let midpoint = (upper + lower) / 2L
        if pred midpoint |> not
        then binarySearch pred (midpoint + 1L) upper
        else binarySearch pred lower midpoint

module Queue =
    type Queue<'a> =
        | Queue of 'a list * 'a list

    let empty = Queue([], [])

    let enqueue e q = 
        match q with
        | Queue(fs, bs) -> Queue(e :: fs, bs)

    let dequeue q = 
        match q with
        | Queue([], []) -> failwith "Empty queue!"
        | Queue(fs, b :: bs) -> b, Queue(fs, bs)
        | Queue(fs, []) -> 
            let bs = List.rev fs
            bs.Head, Queue([], bs.Tail)

    let length q =
        match q with
        | Queue (x, y) -> (List.length x) + (List.length y)

let add2 (a, b) (c, d) = (a + c, b + d)

// Fast modular exponentiation
let expMod a b n =
    let rec loop a b c =
        if b = 0I then c else
            loop (a * a % n) (b >>> 1) (if b &&& 1I = 0I then c else c * a % n)
    loop a b 1I

// Fermat's Little Theorem
let inverseModP p n =
    expMod n (p - 2I) p

let inline (|?) (a: 'a option) (b: 'a option) = if a.IsSome then a else b

let inline addIf condition vs list = if condition then vs @ list else list