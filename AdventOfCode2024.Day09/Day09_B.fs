module Day09_B

open System
open System.IO

type FileLength =
    {
        FileId: int
        FileSize: int
        FreeSpace: int
    }
    
type Block =
    | FileId of int
    | FreeSpace

let run () =
    printfn "Running Day09_B..."
    let filename = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day09_data.txt")
    
    let text = File.ReadAllText(filename).TrimEnd()
    let length = text |> String.length
    
    let diskMap =
        seq { 0..2..(length-1) }
        |> Seq.fold (fun (entries,fileId) idx ->
            let fileSize = text[idx..idx] |> int
            let freeSpace =
                if (idx+1) < length then
                    text[(idx+1)..(idx+1)] |> int
                else
                    0
            let entry = { FileId = fileId ; FileSize = fileSize ; FreeSpace = freeSpace }
            (entry::entries,fileId+1)
            ) ([],0)
        |> fst
        |> List.rev
        
    let totalSize =
        diskMap
        |> List.fold (fun total entry -> total + entry.FileSize + entry.FreeSpace) 0
        
    let disk =
        Array.init totalSize (fun _ -> FreeSpace)
        
    // Populate disk from diskMap
    let writeMapToDisk (disk: Block array) diskMap =
        diskMap
        |> List.fold (fun idx entry ->
            for i in 0..(entry.FileSize-1) do
                disk[idx + i] <- FileId entry.FileId
            idx + entry.FileSize + entry.FreeSpace   
            ) 0
        |> ignore
    
    let printDisk disk =
        disk
        |> Array.map (function
            | FileId fId -> fId.ToString()
            | FreeSpace -> "."
            )
        |> fun xs -> System.String.Join("",xs)
        |> printfn "%s"
    
    let getChecksum disk =
        seq { 0..(Array.length disk)-1 }
        |> Seq.fold (fun sum i ->
            match disk[i] with
            | FreeSpace -> sum
            | FileId fId -> sum + ((int64 i) * (int64 fId))
            ) 0L

    let tryFindIndexI func (data: 'a list) =
        let rec loop idx rem =
            match rem with
            | [] -> None
            | x::xs ->
                if func idx x then
                    Some idx
                else
                    loop (idx+1) xs
        loop 0 data
        
    
    let defragFile fileId diskMap =
        // find location of fileId with index
        let fromIdx = diskMap |> List.findIndex (fun x -> x.FileId = fileId)
        let fromMapping = diskMap[fromIdx]
        // try and find somewhere to move it to
        match diskMap |> tryFindIndexI (fun i x -> x.FreeSpace >= fromMapping.FileSize && i < fromIdx) with
        | Some toIdx ->
            let toMapping = diskMap[toIdx]
            let toMapping' = { toMapping with FreeSpace = 0 }
            let fromMapping' = { fromMapping with FreeSpace = toMapping.FreeSpace - fromMapping.FileSize }
            let diskMap' =
                diskMap
                |> List.updateAt toIdx toMapping'
                |> List.insertAt (toIdx+1) fromMapping'
                |> List.removeAt (fromIdx+1)
            let preceedingMapping = diskMap'[fromIdx] // - 1 + 1 (from insert) = 0
            let preceedingMapping' = { preceedingMapping with FreeSpace = preceedingMapping.FreeSpace + fromMapping.FileSize + fromMapping.FreeSpace }
            let diskMap'' =
                diskMap'
                |> List.updateAt fromIdx preceedingMapping'
            Array.fill disk 0 (Array.length disk) FreeSpace
            diskMap''
        | _ -> diskMap

    
    let defrag diskMap =
        let fileIdsToMode =
            diskMap
            |> List.map _.FileId
            |> List.rev
        
        fileIdsToMode
        |> List.fold (fun dm fileId -> defragFile fileId dm) diskMap
    
    let defraggedDiskMap = defrag diskMap
    
    writeMapToDisk disk defraggedDiskMap

    let result = getChecksum disk
    
    printfn $"Day 09 Part 2:- Result: %i{result}"
    
    ()

