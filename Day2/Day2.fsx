open System
open System.IO

let inputPath = Path.Combine(__SOURCE_DIRECTORY__, "Input.txt")
let inputLines = File.ReadAllLines(inputPath)
let inputLinesTest = [|
    "7 6 4 2 1";
    "1 2 7 8 9";
    "9 7 6 2 1";
    "1 3 2 4 5";
    "8 6 4 4 1";
    "1 3 6 7 9";
|]

let onlyIncreasing (pairs: (int*int) list) =
    pairs
    |> List.forall (fun (n1, n2) -> n2 - n1 > 0)

let onlyDecreasing (pairs: (int*int) list) =
    pairs
    |> List.forall (fun (n1, n2) -> n1 - n2 > 0)

let pairIsSafe (n1:int, n2:int) =
    let increase = Math.Abs(n2 - n1)
    increase > 0 && increase < 4
let increaseIsSafe (pairs: (int*int)list) =
    pairs
    |> List.forall pairIsSafe
let tryReportIsSafe (report:int list) =
    let pairs =
        report
        |> List.pairwise
    let onlyIncreasing = onlyIncreasing pairs
    let onlyDecreasing = onlyDecreasing pairs
    let increaseIsSafe = increaseIsSafe pairs
    (onlyIncreasing || onlyDecreasing) && increaseIsSafe
let reportIsSafe (report:string) =
    let levels =
        report.Split(' ')
        |> Array.toList
        |> List.map int
    if tryReportIsSafe(levels) then
        true
    else
        let isNotSafe =
            [0..levels.Length - 1]
            |> List.forall (fun index ->
                let r = List.removeAt index levels
                not (tryReportIsSafe r)
                )
        not isNotSafe

inputLines
|> Array.map reportIsSafe
|> Array.filter id
|> Array.length