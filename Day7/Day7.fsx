open System
open System.IO

let inputPath = Path.Combine(__SOURCE_DIRECTORY__, "Input.txt")
let inputPathTest = Path.Combine(__SOURCE_DIRECTORY__, "InputTest.txt")
let inputFile = File.ReadAllLines(inputPath)
let input = inputFile

type Equation = {Result: int64; Parameters: int64[]}
type Operator = |Add|Mutiply|Concatenate

let parseEquation (inputLine:string) = 
    let resultParameters = inputLine.Split(": ")
    let result = (int64)resultParameters[0]
    let parameters = 
        resultParameters[1].Split(' ')
        |> Array.map int64
    {Result = result; Parameters = parameters} 

let rec getCombisAddMultiply n =
    if n = 0 then
        [| [| Operator.Add |] |] 
    else
        let smallerCombis = getCombisAddMultiply (n - 1)
        Array.concat [
            smallerCombis |> Array.map (fun arr -> Array.append arr [| Operator.Add |])
            smallerCombis |> Array.map (fun arr -> Array.append arr [| Operator.Mutiply |] )
        ]

let rec getCombisAddMultiplyConcatenation n =
    if n = 0 then
        [| [| Operator.Add |] |] 
    else
        let smallerCombis = getCombisAddMultiplyConcatenation (n - 1)
        Array.concat [
            smallerCombis |> Array.map (fun arr -> Array.append arr [| Operator.Add |])
            smallerCombis |> Array.map (fun arr -> Array.append arr [| Operator.Mutiply |] )
            smallerCombis |> Array.map (fun arr -> Array.append arr [| Operator.Concatenate |] )
        ]

let applyOperatorCombination (equation:Equation) (operatorCombination:Operator array) = 
    let equationParts = Array.zip equation.Parameters operatorCombination
    let applyEquationPart (result:int64) ((parameter,operator):(int64*Operator)) = 
        match operator with
        | Operator.Add -> result + parameter
        | Operator.Mutiply -> result * parameter
        | Operator.Concatenate -> (int64)(result.ToString() + parameter.ToString())
    let result = 
        equationParts
        |> Array.fold applyEquationPart 0
    result

let isPossible generateOperatorCombinations (equation:Equation)  = 
    let operatorCombinations = generateOperatorCombinations (equation.Parameters.Length - 1) 
    operatorCombinations
    |> Array.map (applyOperatorCombination equation)
    |> Array.exists (fun result -> result = equation.Result)

let equations = 
    input
    |> Array.map parseEquation

let (possibleEquationsAddMultiply, restOfEquations) = 
    equations |> Array.partition (isPossible getCombisAddMultiply)

let calibrationResultAddMultiply = 
    possibleEquationsAddMultiply
    |> Array.filter (isPossible getCombisAddMultiply)
    |> Array.sumBy (fun e -> e.Result)

let calibrationResultAddMultiplyConcatenation = 
    restOfEquations
    |> Array.filter (isPossible getCombisAddMultiplyConcatenation)
    |> Array.sumBy (fun e -> e.Result)

let calibrationResult = calibrationResultAddMultiply + calibrationResultAddMultiplyConcatenation
