﻿module Day1

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

let inverseCaptcha (input : string, offset : int) : int =
    input.ToCharArray() |>
    Array.mapi (fun i e -> (i,e)) |> 
    Array.filter (fun (i,e) -> e.Equals(input.Chars((i+offset) % input.Length))) |>
    Array.map (fun (i,e) -> e) |>
    Array.map (fun c -> int c - int '0') |>
    Array.sum