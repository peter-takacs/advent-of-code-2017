module Day15
open Common

let modulus = 2147483647UL;

let generatorA = generate 618UL (fun c -> (c * 16807UL) % modulus)
let generatorB = generate 814UL (fun c -> (c * 48271UL) % modulus)
let generatorA1 = generate 618UL (fun c -> (c * 16807UL) % modulus) |> Seq.where (fun x -> x % 4UL = 0UL)
let generatorB1 = generate 814UL (fun c -> (c * 48271UL) % modulus) |> Seq.where (fun x -> x % 8UL = 0UL)

let matchingLowWordCount a b iterations = 
    Seq.zip a b
    |> Seq.take iterations
    |> Seq.where (fun (x, y) -> (x &&& 0xFFFFUL) = (y &&& 0xFFFFUL))
    |> Seq.length

