open Common
open Day1
open Day2
open Day3
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
    0
