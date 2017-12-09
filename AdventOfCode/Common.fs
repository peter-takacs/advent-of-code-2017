module Common
open System
open System.Text.RegularExpressions
open FSharpx.Collections

let newlines = new Regex("\r?\n")
let spaces = new Regex("\s+")

let toTable (input :string ) =
    newlines.Split input |>    
    Seq.map (spaces.Split >> List.ofSeq)    

let flatten seqOfSeq = seq { for seq in seqOfSeq do yield! seq}

let without e s = Seq.filter (fun x -> x <> e) s

let getOrDefault (m : Map<'a,'b>, key: 'a, def : 'b) = if m.ContainsKey(key) then m.[key] else def; 

let eq a b = a=b
let comp a b = 
    if a > b then 1
    elif a < b then -1
    else 0

let flip f a b = f b a

type FoldCondition<'State, 'Element> = 'State -> 'Element -> bool
let rec generate seed step =
    seq {yield seed; yield! generate (step seed) step}

let rec foldWhile (condition : FoldCondition<'State, 'Element>) (fold : 'State -> 'Element -> 'State) state (sequence : seq<'Element>) =
        if condition state (Seq.head sequence) then Seq.empty
        else seq{yield state; yield! foldWhile condition fold (fold state (Seq.head sequence)) (Seq.tail <| Seq.cache sequence)}
