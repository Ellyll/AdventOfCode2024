module Day12_B

open System
open System.IO

let run () =
    printfn "Running Day12_B..."
    
    let filename = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day12_data_test.txt")
    let lines = File.ReadAllLines(filename)
    let width = lines[0] |> String.length
    let height = lines |> Array.length

    let directions = [
            1,0 // N
            0,1 // E
            -1,0 // S
            0,-1 // W
        ]
    
    let data =
        Array2D.init height width (fun r c -> lines[r][c])
      
    let foldi2D folder initialState arr =
        let mutable state = initialState
        for r in 0 .. (Array2D.length1 arr)-1 do
            for c in 0 .. (Array2D.length2 arr)-1 do
                state <- folder state r c arr[r,c]
        state
    
    let getConnected plantType (r,c) regions data =
        let rec loop connected (rCurr,cCurr) =
            let nextLocations =
                directions
                |> List.map (fun (rd,cd) -> rCurr+rd,cCurr+cd)
                |> List.filter (fun (rn,cn) ->
                    rn >= 0 && rn < Array2D.length1 data &&
                    cn >= 0 && cn < Array2D.length2 data &&
                    data[rn,cn] = plantType &&
                    (not <| Set.contains (rn,cn) connected) &&
                    (not <| List.exists (fun (_,rgn) ->
                        Set.contains (rn,cn) rgn
                        ) regions)                   
                    )           
            nextLocations
            |> List.fold (fun locations (rn,cn) ->
                loop (locations |> Set.add (rn,cn)) (rn,cn)
                ) connected
        loop (Set.singleton (r,c)) (r,c)
    
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
        |> List.rev
    
    let getEdges (plantType,locations) data =
        // Get a list of edges as (Direction, location)
        locations
        |> Set.fold (fun edges (r,c) ->
            let newEdges =
                directions
                |> List.fold (fun es (rd,cd) ->
                    let rn, cn = r+rd, c+cd                   
                    if rn >= 0 && rn < Array2D.length1 data &&
                       cn >= 0 && cn < Array2D.length2 data &&
                       data[rn,cn] = plantType then
                           es
                    else
                        ((rd,cd),(r,c))::es
                    ) []
            newEdges @ edges
            ) []
    
    let getSides edges =
        let northSides =
            edges
            |> List.filter (fun (dir,_) -> dir = (-1,0))
            |> List.sort
            |> List.fold (fun (prev,currSide,sides) (_,(r,c)) ->
                match prev with
                | None -> (Some (r,c)),[(r,c)],sides
                | Some (rPrev,cPrev) ->
                    // Is contiguous
                    if r = rPrev && c = cPrev + 1 then
                        (Some (r,c)),(r,c)::currSide,sides
                    else
                        (Some (r,c)),[(r,c)],currSide::sides
                ) (None,[],[])
            |> fun (_,currSide,sides) -> currSide::sides
        northSides
        // TODO: east, west and south sides
        
    let regions = getRegions data

    let result =
        regions
        |> List.fold (fun totalPrice (plantType, locations) ->
            let edges = getEdges (plantType,locations) data
            let sides = getSides edges
            let area = locations |> Set.count
            let price = area // sides * area
            printfn $"DEBUG: plantType=%c{plantType}, area=%i{area}, sides=%A{sides}, edges=%A{edges}"
            totalPrice + price
            ) 0

    printfn $"Day 12 Part 2:- Result: %i{result}"
    
    ()
