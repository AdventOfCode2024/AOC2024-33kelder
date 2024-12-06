open System
open System.IO

let inputPath = Path.Combine(__SOURCE_DIRECTORY__, "Input.txt")
let inputFile = File.ReadAllLines(inputPath)
let inputTest = [|
    "MMMSXXMASM";
    "MSAMXMSMSA";
    "AMXSXMAAMM";
    "MSAMASMSMX";
    "XMASAMXAMM";
    "XXAMMXXAMA";
    "SMSMSASXSS";
    "SAXAMASAAA";
    "MAMMMXMMMM";
    "MXMXAXMASX"
|]
let input = inputFile

// Part 1
let containsXMAS (line:string) =
    if line.StartsWith("XMAS") || line.StartsWith("SAMX") then Some 1 else None
let getXMASs (line:string) =
    seq { for i = 0 to input.Length - 1 do
          yield (containsXMAS line[i..]) }
    |> Seq.choose id
    |> Seq.length
let getDiagonalTLtoBR (nr:int) (input:string array) =
    let absNr = Math.Abs nr
    if nr < 0 then
        [for i in absNr..input.Length-1 -> input[i][i-absNr]]
        |> String.Concat
    else
        [for i in absNr..input.Length-1 -> input[i-absNr][i]]
        |> String.Concat
let getDiagonalBLtoTR (nr:int) (input:string array) =
    let absNr = Math.Abs nr
    if nr < 0 then
        [for i in absNr..input.Length-1 -> input[input.Length - 1 - i][i-absNr]]
        |> String.Concat
    else
        [for i in absNr..input.Length-1 -> input[input.Length - 1 - i + absNr][i]]
        |> String.Concat
let getHorizontal (nr:int) (input:string array) =
    input[nr]
let getVertical (nr:int) (input:string array) =
    [for i in 0..input.Length-1 -> input[i][nr]]
    |> String.Concat
let horizontalCount =
    [for nr in 0..input.Length - 1 -> getHorizontal nr input]
    |> List.map getXMASs
    |> List.sum
let verticalCount =
    [for nr in 0..input.Length - 1 -> getVertical nr input]
    |> List.map getXMASs
    |> List.sum
let diagonalTLtoBRCount =
    [for nr in -input.Length - 1..input.Length - 1 -> getDiagonalTLtoBR nr input]
    |> List.map getXMASs
    |> List.sum
let diagonalBLtoTRCount =
    [for nr in -input.Length - 1..input.Length - 1 -> getDiagonalBLtoTR nr input]
    |> List.map getXMASs
    |> List.sum
let sumPart1 = horizontalCount + verticalCount + diagonalTLtoBRCount + diagonalBLtoTRCount
 

// Part 2
let flatArray2D array2D =
    seq { for x in [0..(Array2D.length1 array2D) - 1] do
          for y in [0..(Array2D.length2 array2D) - 1] do
            yield array2D.[x, y] }
let invalidChar char =
    not (char = 'M' || char = 'S')
let oppositeTheSame (input:string[]) (row:int) (col:int) =
    (input[row][col] = input[row+2][col+2]) || (input[row+2][col] = input[row][col+2])
let containsMAS  (input:string[]) (row:int) (col:int) =
    if invalidChar (input[row][col]) then None
    else if invalidChar (input[row][col + 2]) then None
    else if invalidChar (input[row + 2][col + 2]) then None
    else if invalidChar (input[row + 2][col]) then None
    else if oppositeTheSame input row col then None
    else Some 1
let innerLetters =
    Array2D.init (input.Length - 2) (input[0].Length - 2) (fun row col -> input[row + 1][col + 1])
let aLettersCoordinates =
    innerLetters
    |> Array2D.mapi (fun row col char -> if char = 'A' then Some (row,col) else None)
let sumPart2 =
    flatArray2D aLettersCoordinates
    |> Seq.choose id
    |> Seq.map (fun (row, col) -> containsMAS input row col)
    |> Seq.choose id
    |> Seq.toList
    |> List.sum