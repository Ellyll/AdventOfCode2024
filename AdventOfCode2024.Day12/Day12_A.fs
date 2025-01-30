module Day12_A

open System
open System.IO

let run () =
    printfn "Running Day12_A..."
    
    let filename = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day12_data.txt")
    let lines = File.ReadAllLines(filename)
    let width = lines[0] |> String.length
    let height = lines |> Array.length

    let data =
        Array2D.init height width (fun r c -> lines[r][c])
      
    let foldi2D folder initialState arr =
        let mutable state = initialState
        for r in 0 .. (Array2D.length1 arr)-1 do
            for c in 0 .. (Array2D.length2 arr)-1 do
                state <- folder state r c arr[r,c]
        state
    
    let getConnected plantType (r,c) regions data =
        let directions = [
            1,0 // N
            0,1 // E
            -1,0 // S
            0,-1 // W
        ]
        let rec loop connected (rCurr,cCurr) =
            let nextLocations =
                directions
                |> List.map (fun (rd,cd) -> rCurr+rd,cCurr+cd)
                |> List.filter (fun (rn,cn) ->
                    rn >= 0 && rn < Array2D.length1 data &&
                    cn >= 0 && cn < Array2D.length2 data &&
                    (not <| Set.contains (rn,cn) connected) &&
                    (not <| List.exists (fun (_,rgn) ->
                        Set.contains (rn,cn) rgn
                        ) regions)                   
                    )

            nextLocations
            |> List.fold (fun locations (rn,cn) ->
                // do stuffs recursively
                locations
                ) []
        // do stuffs
        Set.empty
    
    let getRegions data  =
        data
        |> foldi2D (fun regions r c plantType ->
            // is current already in a region? if not find everything in its region else don't need to do anything 
            let isInRegion =
                regions
                |> List.exists (fun (_,region) ->
                    region |> Set.contains (r,c)
                    )
            if not isInRegion then
                //let region = Set.singleton (r,c)
                let region = getConnected plantType (r,c) regions data
                (plantType,region)::regions
            else
                regions
            ) []
        
    let regions = getRegions data
        
    
    //printfn $"Day 12 Part 1:- Result: %i{result}"
    
    ()

