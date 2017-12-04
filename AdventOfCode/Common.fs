module Common
open System
open System.Text.RegularExpressions

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
