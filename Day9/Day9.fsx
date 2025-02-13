let Empty = -1

type FileMapItem = {Length:int;ID:int}
type DiskMapItem = 
    | FreeMapItem of int
    | FileMapItem of FileMapItem

type Free = {Length:int;Location:int}
type File = {Length:int;ID:int;Location:int}

type DiskItem = 
    | Free of Free
    | File of File

let getDiskMapItem ((character,index):(char *int)) =
    let length = int character - int '0'
    if (index% 2) = 0 then 
        FileMapItem {Length = length; ID = (index/2)}
    else
        FreeMapItem length

let getDiskMap (input:string) = 
    seq {for i in 0..input.Length - 1 do yield (input[i],i) }
    |> Seq.map getDiskMapItem
    |> Seq.toArray

let getDiskSize (diskMap:DiskMapItem array) = 
    diskMap
    |> Array.sumBy (fun diskMapItem -> 
        match diskMapItem with
        | FreeMapItem length -> length
        | FileMapItem fileLength -> fileLength.Length)

let getDisk (diskSize:int) (diskMap: DiskMapItem array) = 
    let disk : int array = Array.zeroCreate diskSize
    let diskItems : DiskItem array = Array.zeroCreate diskMap.Length
    diskMap
    |> Array.fold (fun (diskMapIndex,diskIndex) diskMapItem ->
        match diskMapItem with
        | FreeMapItem length -> 
            Array.fill disk diskIndex length Empty
            let locatedFree = Free {Length = length; Location = diskIndex}
            diskItems[diskMapIndex] <- locatedFree
            (diskMapIndex + 1, diskIndex + length)
        | FileMapItem fileMapItem ->
            Array.fill disk diskIndex fileMapItem.Length fileMapItem.ID
            let file = File {Length = fileMapItem.Length; ID = fileMapItem.ID; Location = diskIndex}
            diskItems[diskMapIndex] <- file
            (diskMapIndex + 1, diskIndex + fileMapItem.Length)
        ) (0,0)
    |> ignore
    (disk, diskItems)

let tryGetLastID (disk:int array) (fromIndex: int) =
    let mutable lastID = None
    for diskIndex = (disk.Length - 1) downto fromIndex + 1 do
        if disk[diskIndex] <> Empty && lastID.IsNone then 
            lastID <- Some (diskIndex, disk[diskIndex])
    lastID
    
let compactDiskPart1 (disk:int array) = 
    for diskIndex in [0..disk.Length - 1] do
        if disk[diskIndex] = Empty then
            tryGetLastID disk diskIndex
            |> Option.map (fun (lastIndex, lastID) ->
                    disk[diskIndex] <- lastID
                    disk[lastIndex] <- Empty
                )
            |> ignore
    
let tryMoveFile (disk:int array) (file:File) = 
    let mutable isNotMoved = true
    for i = 0 to file.Location - 1 do
        if isNotMoved then 
            let mutable thereIsPlace = true
            for j = i to i + file.Length - 1 do
                if disk[j] <> Empty then thereIsPlace <- false
            if thereIsPlace then
                for j = i to i + file.Length - 1 do
                    disk[j] <- file.ID
                for j = file.Location to file.Location + file.Length - 1 do
                    disk[j] <- Empty
                isNotMoved <- false

let compactDiskPart2 (disk:int array) (diskItems : DiskItem array) = 
    for i = diskItems.Length - 1 downto 0 do
        let diskItem = diskItems[i]
        match diskItem with
        | Free _ -> ()
        | File file -> tryMoveFile disk file

let getCheckSum (disk:int array) = 
    let (totalCheckSum, _) =
        disk
        |> Array.fold (fun (checkSum, index) fileID -> 
                if fileID <> Empty then
                    (checkSum + (int64)(fileID * index), index + 1)
                else
                    (checkSum, index + 1)
            ) (0L,0)
    totalCheckSum

let getResultPart1 (input:string array) =
    let diskMap = getDiskMap input[0]
    let diskSize = getDiskSize diskMap
    let (disk, _) = getDisk diskSize diskMap
    compactDiskPart1 disk
    getCheckSum disk

let getResultPart2 (input:string array) =
    let diskMap = getDiskMap input[0]
    let diskSize = getDiskSize diskMap
    let (disk, diskItems) = getDisk diskSize diskMap
    compactDiskPart2 disk diskItems
    getCheckSum disk

let input = System.IO.File.ReadAllLines(System.IO.Path.Combine(__SOURCE_DIRECTORY__, "Input.txt"))
let resultPart1 = getResultPart1 input
let resultPart2 = getResultPart2 input