module Day05_A

open System
open System.IO

let run () =
    printfn "Running Day05_A..."
    let filename = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day05_data.txt")
    
   
    let texts = File.ReadAllText(filename).Split("\n\n", StringSplitOptions.TrimEntries)
    if Array.length texts <> 2 then
        failwithf $"Error parsing %s{filename}"
    
    let rules =
        texts.[0].Split("\n")
        |> Array.map (fun line -> line.Split('|') |> Array.map int)

    let updates =
        texts.[1].Split("\n")
        |> Array.map (fun line -> line.Split(',', StringSplitOptions.TrimEntries)
                                  |> Array.mapi (fun i num -> int num, i)
                                  |> Map.ofArray)
    
    let isUpdateValid rules update =
        rules
        |> Array.forall (fun (rule: int array) ->
            match update |> Map.tryFind rule[0] with
            | None -> true
            | Some order1 ->
                match update |> Map.tryFind rule[1] with
                | None -> true
                | Some order2 ->
                    order1 < order2
            )
            
    let validUpdates =
        updates
        |> Seq.filter (fun update -> isUpdateValid rules update)
    
    let middleNumbers =
        validUpdates
        |> Seq.map (fun update ->
                let len = update |> Map.count
                let mid = len / 2
                update |> Map.findKey (fun _ v -> v = mid)
            )
    
    let result =
        middleNumbers
        |> Seq.sum
    
    //printfn "%A" rules
    //printfn "%A" updates
    
    printfn $"Day 05 Part 1:- Result: %i{result}"
    
    () 

