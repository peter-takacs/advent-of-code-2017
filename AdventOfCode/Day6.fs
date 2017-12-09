module Day6
open Common
open System.Collections.Generic

let d6input = [2;8;8;5;4;2;3;1;5;5;1;2;15;13;5;14]
//let d6input = [0;2;7;0]


let distribute blocks banks offset = 
    let remainder = blocks % banks;
    List.map2 (+)
    <| List.init banks (fun n -> if (n>offset && n <=offset+remainder) || (n<offset && offset+remainder >= banks && n<=(offset+remainder) % banks) then 1 else 0)
    <| List.init banks (fun _ -> blocks/banks) 
 
let step state = 
    let max = (List.max state);
    let maxIndex = List.findIndex ((=) max) state;
    List.map2 (+)
    <| distribute max (List.length state) maxIndex    
    <| List.concat [List.take maxIndex state; [0]; List.skip (maxIndex+1) state]

let compare s1 s2 =
    let zipped = Seq.zip s1 s2
    let firstDifferent = Seq.tryFind (fun c -> c ||> eq |> not) zipped
    match firstDifferent with
    | Some((a,b)) -> if a < b then 1 else -1
    | _ -> 0

[<CustomEquality>]
[<CustomComparison>]
type ComparableSeq<'T> when 'T : comparison = {
    content: seq<'T>
    }with
    interface System.IEquatable<ComparableSeq<'T>> with
        member x.Equals y = eq x.content y.content        
    interface System.IComparable with
        member x.CompareTo y =
            match y with
            | :? ComparableSeq<'T> as l -> compare x.content l.content
            | _ -> 1
    override x.Equals y =
        match y with
            | :? ComparableSeq<'T> as l -> eq x.content l.content 
            | _ -> false

[<CustomEquality>]
[<CustomComparison>]
type NumberedList<'T> when 'T : comparison = {
    index: int;
    content: seq<'T>
    }
    with
    interface System.IEquatable<NumberedList<'T>> with
        member x.Equals y = eq x.content y.content        
    interface System.IComparable with
        member x.CompareTo y =
            match y with
            | :? NumberedList<'T> as l -> compare x.content l.content
            | _ -> 1
    override x.Equals y =
        match y with
            | :? NumberedList<'T> as l -> eq x.content l.content 
            | _ -> false

let numberOfStepsUntilLoop startingState =
    generate startingState step
    |> Seq.mapi (fun i e ->(i,e))
    |> foldWhile (fun s (_,e) -> Map.containsKey e s) (fun s (i,e) -> Map.add e i s) Map.empty
    
let loopLength startingState =
    generate startingState step 
    |> Seq.mapi (fun i e -> (i,e))    
    |> foldWhile (fun s (_,e) -> Map.containsKey e s) (fun s (i,e) -> Map.add e i s) Map.empty
    |> Seq.zip (generate startingState step |> Seq.mapi (fun i e -> (i, e)))
    |> Seq.last
    |> fun ((i,e), m) -> i - (Map.find (step e) m)
