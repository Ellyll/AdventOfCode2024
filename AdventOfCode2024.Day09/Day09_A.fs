module Day09_A

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
    printfn "Running Day09_A..."
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

    let defrag (disk: Block array) =
        let tryFindFreeSpace beforeIdx =
            let rec iter currIdx =
                if currIdx >= beforeIdx then
                    None
                else
                    match disk[currIdx] with
                    | FreeSpace -> Some currIdx
                    | FileId _ -> iter (currIdx + 1)
            iter 0
            
        let rec loop () =
            // Find last non-empty block
            match disk |> Array.tryFindIndexBack (function | FileId _ -> true | FreeSpace -> false) with
            | None -> ()
            | Some fromIdx ->
                match tryFindFreeSpace fromIdx with
                | None -> ()
                | Some toIdx ->
                    disk[toIdx] <- disk[fromIdx]
                    disk[fromIdx] <- FreeSpace
                    loop ()
        loop ()
    
    let getChecksum disk =
        seq { 0..(Array.length disk)-1 }
        |> Seq.fold (fun sum i ->
            match disk[i] with
            | FreeSpace -> sum
            | FileId fId -> sum + ((int64 i) * (int64 fId))
            ) 0L

    let result = getChecksum disk
    
    printfn $"Day 09 Part 1:- Result: %i{result}"
    
    ()

