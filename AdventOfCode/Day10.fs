module Day10

open Common
open System

let d10input = "230,1,2,221,97,252,168,169,57,99,0,254,181,255,235,167";


let extractCircularSublist lst start length =
    let withoutHead = List.skip start lst;
    if List.length withoutHead >= length then List.take length withoutHead
    else
    List.concat [
       withoutHead;
       List.take (length - List.length withoutHead) lst
    ]


let pasteCircular lst start clipboard = 
    let overflowLength = Math.Max(0, start + (List.length clipboard) - (List.length lst));
    let clipHead = List.take (List.length clipboard - overflowLength) clipboard;
    let overflow = List.skip (List.length clipHead) clipboard;
    List.concat [
        overflow;
        List.take start lst |> List.skip overflowLength;
        clipHead;
        skipOrEmpty (start + List.length clipboard) lst
    ]

let reverseCircularSublist lst start length = 
    extractCircularSublist lst start length |> List.rev |> pasteCircular lst start

let twistHash lst lengths =
    Seq.fold (fun (lst, start, skipSize) length -> printfn "%A" <| reverseCircularSublist lst start length; (reverseCircularSublist lst start length, (start + length + skipSize) % List.length lst, skipSize + 1)) (lst, 0, 0) lengths

let doHash (input: string) =
    twistHash (generate 0 (fun x -> x+1) |> Seq.take 256 |> List.ofSeq) (input.Split(',') |> Seq.map int) |> (fun (x, _, _) -> x) |> List.take 2 |> List.reduce (*)