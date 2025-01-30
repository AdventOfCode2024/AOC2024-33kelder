type Location = {X:int; Y:int}
type Antenna = {Freq:char; Loc:Location}
type Antinode = {Loc:Location;Antenna1:Antenna;Antenna2:Antenna}
type MapDimension = {TopLeft:Location;BottomRight:Location}

let locationIsOnMap (mapDimension:MapDimension) (location:Location) = 
    not (location.X < mapDimension.TopLeft.X  || 
         location.Y < mapDimension.TopLeft.Y ||
         location.X > mapDimension.BottomRight.X ||
         location.Y > mapDimension.BottomRight.Y )

let mapCharToAntenna (character:char) (row:int) (col:int) = 
    match character with
    | '.' -> None
    | _ -> Some {Antenna.Freq = character;Antenna.Loc = {X = col;Y = row}}

let getAntennas (input:string array) =
    seq { for i in 0..input.Length - 1 do
            for j in 0..input[0].Length - 1 do
                yield (mapCharToAntenna (input[i][j]) i j) }
    |> Seq.choose id
    |> Seq.toArray

let getFrequencies (antennas: Antenna array) = 
    antennas
    |> Array.map (fun antenna -> antenna.Freq)
    |> Array.distinct

let filterByFrequency (antennas:Antenna array) (frequency:char) =
    antennas |> Array.filter (fun antenna -> antenna.Freq = frequency)

let getAntennaCombis (antennas:Antenna array) = 
    let combis = Array.allPairs antennas antennas
    combis |> Array.filter (fun (a1, a2) -> a1 <> a2)

let getAntinode (antenna1:Antenna,antenna2:Antenna) = 
    {X = 2 * antenna1.Loc.X - antenna2.Loc.X; Y = 2 * antenna1.Loc.Y - antenna2.Loc.Y}

let getAntinodes (mapDimension:MapDimension) (antennas:Antenna array) (frequency:char) = 
    let antennasWithSameFrequency = antennas |> Array.filter (fun antenna -> antenna.Freq = frequency)
    let antennaCombis = getAntennaCombis antennasWithSameFrequency
    antennaCombis 
    |> Array.map getAntinode 
    |> Array.filter (locationIsOnMap mapDimension)

let getResultPart1 (input:string array) =
    let antennas = getAntennas input 
    let frequenties = getFrequencies antennas
    let mapDimension = {TopLeft = {X = 0; Y = 0}; BottomRight = {X = input.Length - 1; Y = input[0].Length - 1}}
    frequenties
    |> Array.map (getAntinodes mapDimension antennas)
    |> Array.concat
    |> Array.distinct
    |> Array.length

let getResultPart2 (input:string array) =
    input.Length

let input = System.IO.File.ReadAllLines(System.IO.Path.Combine(__SOURCE_DIRECTORY__, "InputTest.txt"))
let resultPart1 = getResultPart1 input
let resultPart2 = getResultPart2 input