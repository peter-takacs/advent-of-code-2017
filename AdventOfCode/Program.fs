﻿
open System
open System.Text.RegularExpressions

let d1input = "616976379622766413664" +
    "422972473671177381147198634736481319824497286881167286" +
    "9586657298952447339298296397641114768358841587821418999616"+
    "35335845471757941581181487242988327988983333997865614591526441446"+
    "699598873414819683191729873579897857913667328499327883437721121"+
    "766147238584749599197138553988769564276313541726681335498455856322"+
    "119355736621813316131378698666932593743221698116836353253215972428893"+
    "58147123358117774914653787371368574784376721652181792371635288376729784"+
    "9675268249151925267449351879895713477462221136255779634761419231875346584456"+
    "1559698761438591151393929225726372351877488817463596325462476968453353144374"+
    "5729344341973746469326838186248448483587477563285867499956446218775232374383433"+
    "9218359931364633836288611155731428543589432911487662996536331955821359345449646576"+
    "631983877944424435319646151696552436526967824433946391696878474637215855279478399921"+
    "82415393199964893658322757634675274422993237955354185194868638454891442893935694454"+
    "32423596815591396328264264996815328462615411147838991431676578343436545835278586889"+
    "55824883123349313179356694534474789369385336699211654373737414483784773918127799715"+
    "28975478298688754939216421429251727555596481943322266289527996672856387648674166997"+
    "73134255898657525879326198681717748719751228216296416715125948574483585454751334132"+
    "26477326624435122518867718876516141776792299842711912923747559154573727758561785399"+
    "65131319568278252326242615151412772254257847413799811417287481321745372879513766235"+
    "745347872632946776538173667371228977212143996391617974367923439923774388523845589769"+
    "34135116731139878779758354343472537434361172437939956619743215414688134452831982643"+
    "455423937366696254627129971774359122556756465551135325519751651521396386238376225895"+
    "9957474789718564758843367325794589886852413314713698911855183778978722558742329429867"+
    "2392614647736463894843184465743753236741366384521738151767323854686752152647367862428"+
    "66295648997365412637499692817747937982628518926381939279935993712418938567488289246779"+
    "458432179335139731952167527521377546376518126276"

let d2input = """157	564	120	495	194	520	510	618	244	443	471	473	612	149	506	138
1469	670	47	604	1500	238	1304	1426	54	749	1218	1409	60	51	1436	598
578	184	2760	3057	994	167	2149	191	2913	2404	213	1025	1815	588	2421	3138
935	850	726	155	178	170	275	791	1028	75	781	138	176	621	773	688
212	977	297	645	229	194	207	640	804	509	833	726	197	825	242	743
131	43	324	319	64	376	231	146	382	162	464	314	178	353	123	446
551	121	127	155	1197	288	1412	1285	557	137	145	1651	1549	1217	681	1649
1723	1789	5525	4890	3368	188	3369	4842	3259	2502	4825	163	146	2941	126	5594
311	2420	185	211	2659	2568	2461	231	2599	1369	821	506	2227	180	220	1372
197	4490	141	249	3615	3314	789	4407	169	352	4383	5070	5173	3115	132	3513
4228	2875	3717	504	114	2679	165	3568	3002	116	756	151	4027	261	4813	2760
651	3194	2975	2591	1019	835	3007	248	3028	1382	282	3242	296	270	3224	3304
1858	1650	1720	1848	95	313	500	1776	207	1186	72	259	281	1620	79	77
3841	3217	440	3481	3643	940	3794	4536	1994	4040	3527	202	193	1961	230	217
2837	2747	2856	426	72	78	2361	96	2784	2780	98	2041	2444	1267	2167	2480
411	178	4263	4690	3653	162	3201	4702	3129	2685	3716	147	3790	4888	79	165"""

let inverseCaptcha (input : string, offset : int) : int =
    input.ToCharArray() |>
    Array.mapi (fun i e -> (i,e)) |> 
    Array.filter (fun (i,e) -> e.Equals(input.Chars((i+offset) % input.Length))) |>
    Array.map (fun (i,e) -> e) |>
    Array.map (fun c -> int c - int '0') |>
    Array.sum

let newlines = new Regex("\r?\n")
let spaces = new Regex("\s+")

let toTable (input :string ) =
    newlines.Split input |>
    Seq.map (spaces.Split >> Seq.map int)

let differenceOfExtremes (s : seq<int>) =
    Math.Abs(Seq.max s - Seq.min s)

let flatten seqOfSeq = seq { for seq in seqOfSeq do yield! seq}
let without e s = Seq.filter (fun x -> e <> x) s
let makePairs e s= Seq.map (fun e2 -> if e>=e2 then (e,e2) else (e2,e)) s

let findEvenlyDivisible (s : seq<int>) =
    Seq.map (fun e -> without e s |> makePairs e) s |>    
    flatten |>
    Seq.find (fun (a ,b) -> (a % b) = 0)

let checksum input =
    input |>
    toTable |>
    Seq.map differenceOfExtremes |>
    Seq.sum

let divisibleChecksum input =
    input |>
    toTable |>
    Seq.map findEvenlyDivisible |>
    Seq.map (fun (a,b) -> a/b) |>
    Seq.sum

let nextStepCoordinates (x,y) =
    match (x,y) with
        | (0,0) -> (1,0)
        | (x,y) when x > 0 && y < 0 && x = -y + 1 -> (x, y+1) // bottom right, go up
        | (x,y) when x < 0 && y < 0 && y = x  -> (x + 1, y)    // bottom left, go right
        | (x,y) when x < 0 && y > 0 && x = -y -> (x, y-1)     // top left, go down
        | (x,y) when x > 0 && y > 0 && y = x -> (x-1, y)      // top right, go left
        | (x,y) when y < 0 && x >= y && x <= -y -> (x+1, y)    // bottom row
        | (x,y) when y > 0 && x <= y && -x < y -> (x-1, y)    // top row
        | (x,y) when x > 0 && y <= x && y >= -x -> (x, y+1)     // right column
        | (x,y) when x < 0 && y <= -x && y >= x -> (x, y-1)     // left column

let getOrDefault (m : Map<'a,'b>, key: 'a, def : 'b) = if m.ContainsKey(key) then m.[key] else def; 

let getStoredValue (computed: Map<(int * int), int>, (x,y)) = 
    Seq.allPairs [-1..1] [-1..1] |>
    without (0,0) |>
    Seq.map (fun (offsetX, offsetY) -> getOrDefault (computed, (x + offsetX, y + offsetY), 0)) |>
    Seq.sum
                 
let spiralCoordinates x = 
    Seq.fold (fun (px,py) _ -> nextStepCoordinates(px,py)) (0,0) [1..x-1]   
    
let manhattanDistance (x:int,y:int) = Math.Abs(x) + Math.Abs(y);

let findNextOddQuadraticNumber n =
    Seq.initInfinite (fun i -> (2*i+1,(2*i+1)*(2*i+1)))  |>
    Seq.find (fun (a,aa) -> aa>=n) |>
    fst

let whereIs n = 
    match findNextOddQuadraticNumber(n) with
    | x when x*x = n -> (x/2,-x/2)
    | x when x*x - n <= x - 1  -> (x/2-(x*x-n), -x/2)
    | x when x*x - n >= x-1 && x*x - n < 2*(x-1) -> (-x/2, -x/2 + (x*x - n) - (x-1))
    | x when x*x - n >= 2*(x-1) && x*x - n <= 3*(x-1) -> (-x/2 + (x*x - n) - 2*(x-1), x/2)
    | x when x*x - n > 3*(x-1) -> (x/2, x/2 - (x*x - n) + 3*(x-1))

let findFirstLargerThan input =
    Seq.unfold (fun (computed, i, lastWritten) -> 
        if lastWritten > input then None 
        else Some (getStoredValue (computed, whereIs i), (Map.add (whereIs i) (getStoredValue (computed, whereIs i)) computed, i+1, getStoredValue (computed, whereIs i)))) 
        (Map.add (0,0) 1 Map.empty, 2, 1) |>
    Seq.last



    

[<EntryPoint>]
let main argv = 
    printfn "Day 1/1: %i" (inverseCaptcha (d1input,  1 ))
    printfn "Day 1/2: %i" (inverseCaptcha (d1input,  d1input.Length / 2 ))
    printfn "Day 2/1: %i" (checksum d2input)
    printfn "Day 2/2: %i" (divisibleChecksum d2input)
    printfn "Day 3/1: %i" (whereIs 347991 |> manhattanDistance)
    printfn "Day 3/2: %A" (findFirstLargerThan 347991)
    0
