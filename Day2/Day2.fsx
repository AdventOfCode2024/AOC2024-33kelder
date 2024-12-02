open System
open System.IO

let inputPath = Path.Combine(__SOURCE_DIRECTORY__, "Input.txt")
let inputLines = File.ReadAllLines(inputPath)

let inputLinesTest = [| "7 6 4 2 1";
    "1 2 7 8 9";
    "9 7 6 2 1";
    "1 3 2 4 5";
    "8 6 4 4 1";
    "1 3 6 7 9";
|]

let onlyIncreasingOrDecreasing (pairs: (int*int)[]) =
    let allIncreasing = 
        pairs
        |> Array.map (fun (n1, n2) -> n2 - n1 > 0)
        |> Array.forall id
    let allDecreasing = 
        pairs
        |> Array.map (fun (n1, n2) -> n1 - n2 > 0)
        |> Array.forall id
    allIncreasing || allDecreasing

let increaseIsSafe (n1:int, n2:int) =
    let increase = Math.Abs(n2 - n1)
    increase > 0 && increase < 4

let reportIsSafe (line:string) = 
    let pairs =
        line.Split(' ')
        |> Array.map int
        |> Array.pairwise
    let onlyIncreasingOrDecreasing = onlyIncreasingOrDecreasing pairs
    let increaseIsSafe =
        pairs
        |> Array.map increaseIsSafe
        |> Array.forall (fun b -> b = true)
    onlyIncreasingOrDecreasing && increaseIsSafe

inputLines
|> Array.map reportIsSafe
|> Array.filter (fun b -> b)
|> Array.length
