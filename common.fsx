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