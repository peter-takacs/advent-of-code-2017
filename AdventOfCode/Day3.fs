module Day3
open Common
open System

let nextStepCoordinates (x,y) =
    match (x,y) with
        | (0,0) -> (1,0)
        | (x,y) when x > 0 && y < 0 && x = -y + 1 -> (x, y+1) // bottom right, go up
        | (x,y) when x < 0 && y < 0 && y = x  -> (x + 1, y)    // bottom left, go right
        | (x,y) when x < 0 && y > 0 && x = -y -> (x, y-1)     // top left, go down
        | (x,y) when x > 0 && y > 0 && y = x -> (x-1, y)      // top right, go left
        | (x,y) when y < 0 && x >= y && x <= -y -> (x+1, y)    // bottom row
        | (x,y) when y > 0 && x <= y && -x < y -> (x-1, y)    // top row
        | (x,y) when x > 0 && y <= x && y >= -x -> (x, y+1)     // right column
        | (x,y) when x < 0 && y <= -x && y >= x -> (x, y-1)     // left column


let getStoredValue (computed: Map<(int * int), int>, (x,y)) = 
    Seq.allPairs [-1..1] [-1..1] |>
    without (0,0) |>
    Seq.map (fun (offsetX, offsetY) -> getOrDefault (computed, (x + offsetX, y + offsetY), 0)) |>
    Seq.sum
                 
let spiralCoordinates x = 
    Seq.fold (fun (px,py) _ -> nextStepCoordinates(px,py)) (0,0) [1..x-1]   
    
let manhattanDistance (x:int,y:int) = Math.Abs(x) + Math.Abs(y);

let findNextOddQuadraticNumber n =
    Seq.initInfinite (fun i -> (2*i+1,(2*i+1)*(2*i+1)))  |>
    Seq.find (fun (a,aa) -> aa>=n) |>
    fst

let whereIs n = 
    match findNextOddQuadraticNumber(n) with
    | x when x*x = n -> (x/2,-x/2)
    | x when x*x - n <= x - 1  -> (x/2-(x*x-n), -x/2)
    | x when x*x - n >= x-1 && x*x - n < 2*(x-1) -> (-x/2, -x/2 + (x*x - n) - (x-1))
    | x when x*x - n >= 2*(x-1) && x*x - n <= 3*(x-1) -> (-x/2 + (x*x - n) - 2*(x-1), x/2)
    | x when x*x - n > 3*(x-1) -> (x/2, x/2 - (x*x - n) + 3*(x-1))

let findFirstLargerThan input =
    Seq.unfold (fun (computed, i, lastWritten) -> 
        if lastWritten > input then None 
        else Some (getStoredValue (computed, whereIs i), (Map.add (whereIs i) (getStoredValue (computed, whereIs i)) computed, i+1, getStoredValue (computed, whereIs i)))) 
        (Map.add (0,0) 1 Map.empty, 2, 1) |>
    Seq.last

