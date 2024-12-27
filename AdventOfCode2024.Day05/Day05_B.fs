module Day05_B

open System
open System.IO

let run () =
    printfn "Running Day05_B..."
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
            
    let invalidUpdates =
        updates
        |> Seq.filter (fun update -> not <| isUpdateValid rules update)
    
    let reorderUpdate rules update =
        
        let rec loop update' =
            let update'' =
                rules
                |> Array.fold (fun upd (rule: int array) ->
                    match upd |> Map.tryFind rule[0] with
                    | None -> upd
                    | Some order1 ->
                        match upd |> Map.tryFind rule[1] with
                        | None -> upd
                        | Some order2 ->
                            // If order is wrong, then move first before second
                            if order1 > order2 then
                                let lst =
                                    upd
                                    |> Map.toList
                                    |> List.sortBy snd
                                    |> List.map fst
                                // Move value in wrong location to just before where it needs to be
                                // e.g. if e needs to come before c then a,b,c,d,e,f -> a,b,e,c,d,f 
                                lst
                                |> List.removeAt order1
                                |> List.insertAt order2 rule[0]
                                |> List.mapi (fun i v -> v,i)
                                |> Map.ofList
                            else
                                upd
                    ) update'
            if isUpdateValid rules update'' then
                update''
            else
                loop update''
        loop update
    
    let reorderedUpdates =
        invalidUpdates
        |> Seq.map (reorderUpdate rules)
    
    let middleNumbers =
        reorderedUpdates
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
    
    printfn $"Day 05 Part 2:- Result: %i{result}"
    
    () 

