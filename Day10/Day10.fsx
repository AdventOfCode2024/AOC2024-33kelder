type Location = {X:int; Y:int}
type Map = string array
type Trail = {Head:Location;End:Location}
let getMaxX (map:Map) = map[0].Length - 1
let getMaxY (map:Map) = map.Length - 1
let getChar (digit:int) = (string digit |> Seq.head)

let mapCharTrailHead (character:char) (row:int) (col:int) = 
    match character with
    | '0' -> Some {X = col;Y = row}
    | _ -> None

let getTrailHeads (map:Map) =
    seq { for i in 0..map.Length - 1 do
            for j in 0..map[0].Length - 1 do
                yield (mapCharTrailHead (map[i][j]) i j) }
    |> Seq.choose id
    |> Seq.toArray

let getNextTrailSteps (map:Map) (height:char) (step:Location) =
    let mutable nextSteps : Location array = [||]
    if step.X > 0 && map[step.Y][step.X - 1] = height then nextSteps <- Array.append nextSteps [|{X = step.X - 1; Y = step.Y}|]
    if step.X < getMaxX map && map[step.Y][step.X + 1] = height then nextSteps <- Array.append nextSteps [|{X = step.X + 1; Y = step.Y}|]
    if step.Y > 0 && map[step.Y - 1][step.X] = height then nextSteps <- Array.append nextSteps [|{X = step.X; Y = step.Y - 1}|]
    if step.Y < getMaxY map && map[step.Y + 1][step.X] = height then nextSteps <- Array.append nextSteps [|{X = step.X; Y = step.Y + 1}|]
    nextSteps

let getTrails (map:Map) (trailHead:Location) : Trail array =
    let mutable trailSteps : Location array = [|trailHead|]
    for height in 1..9 do
        let nextTrailSteps = 
            trailSteps
            |> Array.map (getNextTrailSteps map (getChar height))
            |> Array.concat
        trailSteps <- nextTrailSteps
    trailSteps
    |> Array.map (fun trailEnd -> {Head = trailHead; End = trailEnd})

let getScore (map:Map) (trailHead:Location) =
    let trails = getTrails map trailHead
    let distinctTrailEnds = 
        trails 
        |> Array.map (fun trail -> trail.End) 
        |> Array.distinct
    distinctTrailEnds.Length

let getRating (map:Map) (trailHead:Location) =
    let trails = getTrails map trailHead
    trails.Length

let getResultPart1 (map:string array) =
    let trailHeads = getTrailHeads map
    trailHeads
    |> Array.sumBy (getScore map)

let getResultPart2 (map:string array) =
    let trailHeads = getTrailHeads map
    trailHeads
    |> Array.sumBy (getRating map)

let input = System.IO.File.ReadAllLines(System.IO.Path.Combine(__SOURCE_DIRECTORY__, "Input.txt"))
let resultPart1 = getResultPart1 input
let resultPart2 = getResultPart2 input