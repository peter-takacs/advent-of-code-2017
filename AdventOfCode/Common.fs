module Common
open System
open System.Text.RegularExpressions
open FSharpx.Collections
open FParsec

let newlines = new Regex("\r?\n")
let spaces = new Regex("\s+")

let isAsciiIdStart c =
    isAsciiLetter c || c = '_'

let isAsciiIdContinue c =
    isAsciiLetter c || isDigit c || c = '_' || c = '\''

let parsename : Parser<string, unit> = identifier (IdentifierOptions(isAsciiIdStart = isAsciiIdStart, isAsciiIdContinue = isAsciiIdContinue))

let toTable (input :string ) =
    newlines.Split input |>    
    Seq.map (spaces.Split >> List.ofSeq)    

let flatten seqOfSeq = seq { for seq in seqOfSeq do yield! seq}

let rec values l = 
    match l with
    | [] -> []
    | Some(x) :: t -> x :: values t
    | None :: t -> values t

let without e s = Seq.filter (fun x -> x <> e) s

let getOrDefault (m : Map<'a,'b>, key: 'a, def : 'b) = if m.ContainsKey(key) then m.[key] else def; 

let parseOrException parser input = 
    let output = run parser input;
    match output with
    | Success(result, _, _) -> result;
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg; raise (new Exception(errorMsg));

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

let rec foldMap (fold: 'State -> 'Element -> 'State) state (sequence : seq<'Element>) : seq<'Element * 'State> =
    seq{yield (Seq.head sequence, state); yield! foldMap fold (fold state (Seq.head sequence)) (Seq.tail <| Seq.cache sequence)}

let skipOrEmpty count lst = 
    if count < List.length lst then List.skip count lst
    else []
