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
    | Obstruction
    | StartPosition of Coordinate * Direction
    | NotPassed

let mapChar (character:char) (row:int) (col:int) = 
    match character with
    | '^' -> Position.StartPosition ({Row = row; Col = col}, Direction.Up)
    | '#' -> Position.Obstruction
    | _ -> Position.NotPassed

let map =
    Array2D.init input.Length input[0].Length (fun row col -> mapChar(input[row][col]) row col)

let startPosition = 
    map
    |> Seq.cast<Position>
    |> Seq.map (function
        | Position.StartPosition (coordinate, direction) -> (Some (coordinate, direction))
        | _ -> None)
    |> Seq.choose id
    |> Seq.head

let getNextCoordinate (coordinate, direction) = 
    match direction with
    | Direction.Up -> {Row = coordinate.Row - 1; Col = coordinate.Col}
    | Direction.Right -> {Row = coordinate.Row; Col = coordinate.Col + 1}
    | Direction.Down -> {Row = coordinate.Row + 1; Col = coordinate.Col}
    | Direction.Left -> {Row = coordinate.Row; Col = coordinate.Col - 1}

let getNextDirection direction = 
    match direction with 
    | Direction.Up -> Direction.Right
    | Direction.Right -> Direction.Down
    | Direction.Down -> Direction.Left
    | Direction.Left -> Direction.Up

let coordinateIsOutsideMap (map: Position array2d) (coordinate:Coordinate) = 
    coordinate.Row < 0 || 
    coordinate.Row >= Array2D.length1(map)  ||
    coordinate.Col < 0 ||
    coordinate.Col >= Array2D.length2(map) 

let positionIsObstructed (map: Position array2d) (coordinate : Coordinate) =
    match map[coordinate.Row, coordinate.Col] with
    | Position.Obstruction -> true
    | _ -> false

let addPositionToPath (path : (Coordinate * Direction) list) ((coordinate, direction): Coordinate * Direction)  =
    (coordinate, direction) :: path

let guardHasWalkedHere (coordinate:Coordinate) (path : (Coordinate * Direction) list) = 
    path 
    |> List.exists (fun (c,d) -> c = coordinate)

let guardHasWalkedThisDirection ((coordinate,direction):Coordinate * Direction) (path : (Coordinate * Direction) list) = 
    path 
    |> List.exists (fun (c,d) -> c = coordinate && d = direction)

let tryGetNextCoordinateAsLoopObstruction (map: Position array2d) ((currentCoordinate, currentDirection):(Coordinate * Direction)) (path : (Coordinate * Direction) list) =
    let mutable isInLoop = false
    let potentialLoopObstruction = getNextCoordinate (currentCoordinate, currentDirection)
    if map[potentialLoopObstruction.Row, potentialLoopObstruction.Col] = Position.Obstruction || guardHasWalkedHere potentialLoopObstruction path then
        None
    else
        let extraObstacleMap = Array2D.copy map
        extraObstacleMap[potentialLoopObstruction.Row, potentialLoopObstruction.Col] <- Position.Obstruction
        let mutable extraObstaclePath = path
        let mutable guardIsOnMap = true
        let mutable currentPosition = (currentCoordinate, currentDirection)
        let mutable addCurrentPositionToPath = false
        while guardIsOnMap && not isInLoop do
            let (coordinate, direction) = currentPosition
            if addCurrentPositionToPath then
                extraObstaclePath <- addPositionToPath extraObstaclePath (coordinate, direction)
            let nextCoordinate = getNextCoordinate (coordinate, direction)
            if coordinateIsOutsideMap extraObstacleMap nextCoordinate then
                guardIsOnMap <- false
            else if not (positionIsObstructed extraObstacleMap nextCoordinate) then
                currentPosition <- (nextCoordinate, direction)
                addCurrentPositionToPath <- true
            else
                let nextDirection = getNextDirection direction
                currentPosition <- (coordinate, nextDirection)
                addCurrentPositionToPath <- false

            if guardIsOnMap then
                isInLoop <- guardHasWalkedThisDirection currentPosition extraObstaclePath
        if isInLoop then
            Some potentialLoopObstruction
        else
            None

let getLoopObstructions (map: Position array2d) (startPosition:(Coordinate * Direction)) =
    let mutable guardIsOnMap = true
    let mutable currentPosition = startPosition
    let mutable addCurrentPositionToPath = true
    let mutable path : (Coordinate * Direction) list = []
    let mutable loopObstructions : Coordinate list = []
    while guardIsOnMap do
        let (coordinate, direction) = currentPosition
        if addCurrentPositionToPath then
            path <- addPositionToPath path (coordinate, direction)
        let nextCoordinate = getNextCoordinate (coordinate, direction)
        if coordinateIsOutsideMap map nextCoordinate then
            guardIsOnMap <- false
        else if not (positionIsObstructed map nextCoordinate) then
            match tryGetNextCoordinateAsLoopObstruction map (coordinate, direction) path with
                | Some coordinate -> loopObstructions <- coordinate :: loopObstructions
                | _ -> ()
            currentPosition <- (nextCoordinate, direction)
            addCurrentPositionToPath <- true
        else
            let nextDirection = getNextDirection direction
            currentPosition <- (coordinate, nextDirection)
            addCurrentPositionToPath <- false
    loopObstructions

let loopObstructions = 
    getLoopObstructions map startPosition
    |> List.length
