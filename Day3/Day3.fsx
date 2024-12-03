open System
open System.IO

let inputPath = Path.Combine(__SOURCE_DIRECTORY__, "Input.txt")
let input = File.ReadAllText(inputPath)
let inputTest = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))";

let (|IsInteger|_|) (input:string) =
    match System.Int32.TryParse(input) with
    | true, i -> Some i
    | false, _ -> None

type Statement =
    | Multiplication of int
    | Do
    | Dont

let getStatement (input: string) =
    if input.StartsWith("mul") then
        let startIndex = input.IndexOf('(')
        let endIndex = input.IndexOf(')')
        if startIndex = 3 && endIndex > startIndex then
            let argumentString = input.Substring(startIndex + 1, endIndex - startIndex - 1)
            let arguments = argumentString.Split(',')
            if arguments.Length = 2 then
                match (arguments[0], arguments[1]) with
                | (IsInteger i, IsInteger j) -> Some (Multiplication (i * j))
                | _ -> None
            else None
        else None
    else if input.StartsWith("don't()") then
        Some Statement.Dont
    else if input.StartsWith("do()") then
        Some Statement.Do
    else
        None

let getStatements (input: string) =
    seq { for i = 0 to input.Length - 1 do
          yield (getStatement input[i..]) }
    |> Seq.choose id
 
let addEnabledMultiplication (enabled:bool, sum:int) (s:Statement) =
    match s with
    | Multiplication m ->
        if enabled then (enabled, sum + m)
        else (enabled, sum)
    | Do -> (true, sum)
    | Dont -> (false, sum)

let sumOfMultiplications (input: string) =
    let statements = getStatements input
    statements
    |> Seq.fold addEnabledMultiplication (true, 0)

sumOfMultiplications input