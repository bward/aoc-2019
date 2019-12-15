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
        if pred midpoint
        then binarySearch pred midpoint upper
        else binarySearch pred lower (midpoint - 1L)

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

let add2 (a, b) (c, d) = (a + c, b + d)