module Day13

open FParsec;
open Common;

let d13input = 
    """0: 3
    1: 2
    2: 4
    4: 6
    6: 4
    8: 6
    10: 5
    12: 8
    14: 8
    16: 6
    18: 8
    20: 6
    22: 10
    24: 8
    26: 12
    28: 12
    30: 8
    32: 12
    34: 8
    36: 14
    38: 12
    40: 18
    42: 12
    44: 12
    46: 9
    48: 14
    50: 18
    52: 10
    54: 14
    56: 12
    58: 12
    60: 14
    64: 14
    68: 12
    70: 17
    72: 14
    74: 12
    76: 14
    78: 14
    82: 14
    84: 14
    94: 14
    96: 14""";

let layer = pint32 .>> pstring ": " .>>. pint32 .>> FParsec.CharParsers.spaces;
let firewall = many layer;

type Layer = {
    range: int;
    currentPosition: int;
    isAscending: bool;
}

let maxLayer firewall = 
    Seq.map fst firewall |> Seq.max

let parseInput = 
    parseOrException firewall

let toMap firewall =
    firewall |>
    Seq.fold (fun state (depth, range) -> Map.add depth {range = range; currentPosition = 0; isAscending = true} state) Map.empty

let updateLayer layer: Layer =
    match layer with
    | {Layer.range = range; Layer.currentPosition = current; isAscending = _} when current = range - 1 -> {range = range; currentPosition = current - 1; isAscending = false};
    | {Layer.range = range; Layer.currentPosition = 0; isAscending = _} -> {range = range; currentPosition = 1; isAscending = true};
    | {Layer.range = range; Layer.currentPosition = current; isAscending = true} -> {range = range; currentPosition = current + 1; isAscending = true};
    | {Layer.range = range; Layer.currentPosition = current; isAscending = false} -> {range = range; currentPosition = current - 1; isAscending = false};

    

let updateLayers layers: Map<int, Layer> =
    Map.map (fun _ value -> updateLayer value) layers

let generateTicks startingState count = 
    Seq.take count <| generate startingState updateLayers

let computeSeverity delay input =
    let layers = parseInput input
    let count = maxLayer layers + delay
    let firewall = toMap layers
    Seq.init count id |>
    Seq.zip (generateTicks firewall count) |>
    Seq.map (fun (layers, tick) -> 
        match getOrDefault(layers, tick - delay, {Layer.range = 0; Layer.currentPosition = 0; Layer.isAscending = false}) with
        | {Layer.range = range; Layer.currentPosition = 0;} -> tick * range
        | _ -> 0 ) |>
    Seq.sum

let findFirstHole input = 
    Seq.initInfinite id |>
    Seq.map (fun x -> computeSeverity x input) |>
    Seq.findIndex (eq 0)
    
