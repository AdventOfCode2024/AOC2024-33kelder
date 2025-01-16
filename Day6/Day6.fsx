open System
open System.IO

let inputPath = Path.Combine(__SOURCE_DIRECTORY__, "Input.txt")
let inputPathTest = Path.Combine(__SOURCE_DIRECTORY__, "InputTest.txt")
let inputFile = File.ReadAllLines(inputPathTest)
let input = inputFile

type Direction = 
    | Up
    | Right
    | Down
    | Left
type Coordinate = {Row:int; Col:int}
type Position = 
    | CurrentPosition of Coordinate * Direction
    | Obstruction
    | Passed
    | NotPassed

let mapChar (character:char) (row:int) (col:int) = 
    match character with
    | '^' -> Position.CurrentPosition ({Row = row; Col = col}, Direction.Up)
    | '#' -> Position.Obstruction
    | 'X' -> Position.Passed
    | _ -> Position.NotPassed

let map =
    Array2D.init input.Length input[0].Length (fun row col -> mapChar(input[row][col]) row col)

let startPosition = 
    map
    |> Seq.cast<Position>
    |> Seq.map (function
        | Position.CurrentPosition (position, direction) -> (Some (position, direction))
        | _ -> None)
    |> Seq.choose id
    |> Seq.head

let tryGetNextCoordinate (position, direction) = 
    match direction with
    | Direction.Up -> {Row = position.Row - 1; Col = position.Col}
    | Direction.Right -> {Row = position.Row; Col = position.Col + 1}
    | Direction.Down -> {Row = position.Row + 1; Col = position.Col}
    | Direction.Left -> {Row = position.Row; Col = position.Col - 1}

let getNextDirection direction = 
    match direction with 
    | Direction.Up -> Direction.Right
    | Direction.Right -> Direction.Down
    | Direction.Down -> Direction.Left
    | Direction.Left -> Direction.Up

let coordinateIsOutsideMap (map: Position array2d) (coordinate:Coordinate) = 
    coordinate.Row < 0 || 
    coordinate.Row >= Array2D.length1(map) - 1 ||
    coordinate.Col < 0 ||
    coordinate.Col >= Array2D.length2(map) - 1

let positionIsObstructed (map: Position array2d) (position : Coordinate) =
    match map[position.Row, position.Col] with
    | Position.Obstruction -> true
    | _ -> false

let addPositionToPath (map: Position array2d) (position : Coordinate) =
    map[position.Row, position.Col] <- Position.Passed

let walkTheGuard (map: Position array2d) (startPosition:(Coordinate * Direction)) =
    let mutable guardIsOnMap = true
    let mutable currentPosition = startPosition
    while guardIsOnMap do
        let (coordinate, direction) = currentPosition
        addPositionToPath map coordinate
        let nextCoordinate = tryGetNextCoordinate (coordinate, direction)
        if coordinateIsOutsideMap map nextCoordinate then
            guardIsOnMap <- false
        else if not (positionIsObstructed map nextCoordinate) then
            currentPosition <- (nextCoordinate, direction)
        else
            let nextDirection = getNextDirection direction
            currentPosition <- (coordinate, nextDirection)

walkTheGuard map startPosition

map
|> Seq.cast<Position>
|> Seq.map (function
    | Position.Passed -> Some 1
    | _ -> None)
|> Seq.choose id
|> Seq.sum
