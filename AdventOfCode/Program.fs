open Common
open Day1
open Day2
open Day3
open Day4
open Day5
open Day6
open Day7
open Day8
open Day9
open Day10
open System
open System.Text.RegularExpressions

    

[<EntryPoint>]
let main argv = 
    printfn "Day 1/1: %i" (inverseCaptcha (d1input,  1 ))
    printfn "Day 1/2: %i" (inverseCaptcha (d1input,  d1input.Length / 2 ))
    printfn "Day 2/1: %i" (checksum d2input)
    printfn "Day 2/2: %i" (divisibleChecksum d2input)
    printfn "Day 3/1: %i" (whereIs 347991 |> manhattanDistance)
    printfn "Day 3/2: %i" (findFirstLargerThan 347991)
    printfn "Day 4/1: %i" (d4input |> toTable |> withoutDuplicatesInRows |> Seq.length)
    printfn "Day 4/2: %i" (d4input |> toTable |> withoutAnagramRows |> Seq.length)
    printfn "Day 5/1: %i" (Array.copy d5input |> iterateUntilOut (fun i -> i+1))
    printfn "Day 5/2: %i" (Array.copy d5input |> iterateUntilOut (fun i -> if i>2 then i-1 else i+1))
    printfn "Day 6/1: %i" <| (Seq.length  <| numberOfStepsUntilLoop d6input)    
    printfn "Day 6/2: %i" <| (loopLength d6input |> (+) <| 1)
    printfn "Day 7/1: %s" <| (d7input |> parseLines |> findRoot |> (fun (n: Node) -> n.Name))
    printfn "Day 7/2: %i" <| (d7input |> parseLines |> getImbalancedWeight)
    printfn "Day 8/1: %i" <| (d8input |> parse |> getLargestRegister)
    printfn "Day 8/2: %i" <| (d8input |> parse |> getLargestNumberHeld)
    printfn "Day 9/1: %i" <| (d9input |> parseGroups |> calculateScore 1)
    printfn "Day 9/2: %i" <| (d9input |> parseGroups |> calculateGarbageLength)
    printfn "Day 10/1: %i" <| (doHash (d10input.Split(',') |> Seq.map int) |> List.take 2 |> List.reduce (*))
    printfn "Day 10/2: %s" <| (hashText d10input)
    0
