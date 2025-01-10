open System
open System.IO

let inputPath = Path.Combine(__SOURCE_DIRECTORY__, "Input.txt")
let inputPathTest = Path.Combine(__SOURCE_DIRECTORY__, "InputTest.txt")
let inputFile = File.ReadAllLines(inputPath)
let input = inputFile

type PrintInstruction = 
    | OrderingRule of int*int
    | Update of int[]

let parseInputLine (line:string) = 
    if line.Contains('|') then 
        let pages = line.Split('|') |> Array.map int
        Some (PrintInstruction.OrderingRule (pages[0],pages[1]))
    else if line.Contains(',') then
        let pages = line.Split(',') |> Array.map int
        Some (PrintInstruction.Update pages)
    else None

let printInstructions =
    input
    |> Array.map parseInputLine
    |> Array.choose id

let orderingRules = 
    printInstructions
    |> Array.choose (fun pi -> 
        match pi with 
        | PrintInstruction.OrderingRule (p1,p2) -> Some (p1,p2)
        | _ -> None)
let updates = 
    printInstructions
    |> Array.choose (fun pi -> 
        match pi with 
        | PrintInstruction.Update update -> Some update 
        | _ -> None)

let isValidUpdateForRule (update:int[]) (pageBefore:int, pageAfter:int) =
    let indexOfPageBefore = update |> Array.tryFindIndex (fun x -> x = pageBefore)
    let indexOfPageAfter = update |> Array.tryFindIndex (fun x -> x = pageAfter)
    match (indexOfPageBefore, indexOfPageAfter) with
    | (Some ib, Some ia) -> ib < ia
    | _ -> true

let isValidUpdate (update:int[]) (orderingRules:(int*int)[]) = 
    orderingRules |> Array.forall (isValidUpdateForRule update)

let swapPages (update: int[]) i j =
    if i < 0 || j < 0 || i >= update.Length || j >= update.Length then
        invalidArg "Index out of range" "Indices must be within array bounds."
    else
        let temp = update.[i]
        update.[i] <- update.[j]
        update.[j] <- temp

let orderingRuleExists (orderingRule:int*int) (orderingRules:(int*int)[]) = 
    Array.exists (fun rule -> rule = orderingRule) orderingRules

let getViolatingRules (update:int[]) (orderingRules:(int*int)[]) : (int*int)[] = 
    orderingRules |> Array.filter (fun orderingRule -> not (isValidUpdateForRule update orderingRule))

let isInvalidUpdate (update:int[]) (orderingRules:(int*int)[]) = 
    let violatingRules = getViolatingRules update orderingRules
    if violatingRules.Length = 0 then false else true

let sumValidUpdates = 
    updates
    |> Array.filter (fun update -> isValidUpdate update orderingRules)
    |> Array.map (fun update ->
            let middle = (update.Length/2)
            update[middle]
        )
    |> Array.sum

let fixUpdate (update:int[]) (orderingRules:(int*int)[]) =
    let mutable updateIsInvalid = true
    while updateIsInvalid do
        for i in 0 .. update.Length - 1 do
            for j in i + 1 .. update.Length - 1 do
                if orderingRuleExists (update[j], update[i]) orderingRules then
                    swapPages update i j
                if not (isInvalidUpdate update orderingRules) then
                    updateIsInvalid <- false
    update

let sumInvalidUpdates =
    updates
    |> Array.filter (fun update -> isInvalidUpdate update orderingRules)
    |> Array.map (fun update -> fixUpdate update orderingRules)
    |> Array.map (fun update ->
        let middle = (update.Length/2)
        update[middle]
    )
    |> Array.sum



