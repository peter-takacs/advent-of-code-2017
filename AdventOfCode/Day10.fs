module Day10

open Common
open System

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
        List.skip (start + List.length clipboard) lst
    ]

let reverseCircularSublist lst start length = 
    extractCircularSublist lst start length |> List.rev |> pasteCircular lst start