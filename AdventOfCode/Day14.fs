module Day14

open Common;
open Day10;
open System

let d14input = "ffayrhll";

let hashLine line =
    repeatedHash line
    |> toDenseHash
    |> Seq.map (fun (x: int) -> Convert.ToString(x, 2).PadLeft(8, '0'))
    |> (fun x -> String.Join("", x))

let hashLines count input = 
   Seq.init count id
   |> Seq.map (fun i -> input + "-" + i.ToString())
   |> Seq.map hashLine
   |> List.ofSeq

let used (lines: seq<string>) =
    lines
    |> Seq.map (fun l -> l.ToCharArray()) 
    |> Seq.map (fun l -> Seq.where (eq '1') l |> Seq.length)
    |> Seq.sum

let toArray (lines: seq<string>) =
    lines
    |> Seq.map (fun l -> l.ToCharArray())
    |> Seq.map (Seq.map (fun x -> if x = '1' then 1 else 0))
    |> array2D

let enumeratePoints heigth width =
    seq { for x in 0..width-1 -> seq {for y in 0..heigth-1 -> (x, y)}}
    |> Seq.concat
    
let clamp size (x, y) =
    (max x 0 |> min (size-1), max y 0 |> min (size-1))

let rec fillFrom array size (x, y) currentRegion =
    match Array2D.get array x y with
    | 1 -> 
        let mutable state = (array, currentRegion);
        Array2D.set array x y currentRegion;
        state <- fillFrom array size (clamp size (x+1, y)) currentRegion;
        state <- fillFrom array size (clamp size (x-1, y)) currentRegion;
        state <- fillFrom array size (clamp size (x, y+1)) currentRegion;
        state <- fillFrom array size (clamp size (x, y-1)) currentRegion;
        (array, currentRegion + 1)
    | _ -> (array, currentRegion)

let fill size array = 
    let mutable state = (array, 2);
    for (x, y) in enumeratePoints size size do
       state <- fillFrom array size (x, y) (snd state);
    snd state

    
