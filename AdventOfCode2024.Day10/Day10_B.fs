module Day10_B

open System
open System.IO
type Height =
    | Impassable
    | Value of int

let run () =
    printfn "Running Day10_B..."

    let filename = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day10_data.txt")
    let text = File.ReadAllText(filename)

    let lines = text.Trim().Split("\n")
    let width = lines[0] |> String.length
    let height = lines |> Array.length
       
    let data =
        Array2D.init height width (fun l c ->
            let h = lines[l][c]
            if h = '.' then
                Impassable
            elif Char.IsDigit(h) then
                h.ToString() |> int |> Value
            else
                failwithf $"Invalid character %c{h} at (%i{l},%i{c})"
            ) 
    
    let array2dFold folder initalState arr2d =
        seq {
            for l in 0..((Array2D.length1 arr2d)-1) do
                for c in 0..((Array2D.length2 arr2d)-1) -> (l,c)
        }
        |> Seq.fold (fun state (l,c) -> folder state (l,c) arr2d[l,c]) initalState
        
    let getTrailStarts data =
        data
        |> array2dFold (fun state (l,c) value ->
            match value with
            | Value 0 -> state |> Set.add (l,c)
            | _ -> state
            ) Set.empty
    
    let getTrailEnds data =
        data
        |> array2dFold (fun state (l,c) value ->
            match value with
            | Value 9 -> state |> Set.add (l,c)
            | _ -> state
            ) Set.empty
    
    let getValidNextLocations (l,c) (data: Height[,]) =
        let currHeight =
            match data[l,c] with
            | Value h -> h
            | Impassable -> failwithf $"Invalid data, expecting height Value but got Impassable at (%i{l},%i{c})"

        [ // NESW
            (-1,0)
            (0,1)
            (1,0)
            (0,-1)
        ]
        |> List.map (fun (ld,cd) -> l+ld,c+cd)
        |> List.filter (fun (ln,cn) ->
            // Check in bounds and that height rises by 1
            ln >= 0 && ln < (Array2D.length1 data) &&
               cn >= 0 && cn < (Array2D.length2 data) &&
               (not (ln = l && cn = c)) &&
               match data[ln,cn] with
               | Value height when height - currHeight = 1 -> true
               | _ -> false
            )
    
    let findPaths startLocation endLocation (data: Height[,]) : ((int*int) list) list =
        let rec fp visited pathsAcc fromLocation toLocation =
            if fromLocation = toLocation then
                pathsAcc
                |> List.map (fun ps -> fromLocation::ps |> List.rev)
            else
                let visited' = visited |> Set.add fromLocation
                let pathsAcc' =
                    if List.isEmpty pathsAcc then
                        [ [ fromLocation ] ]
                    else
                        pathsAcc |> List.map (fun path -> fromLocation::path)
                let nextLocations =
                    getValidNextLocations fromLocation data
                    |> List.filter (fun nextLocation -> not (Set.contains nextLocation visited))
                nextLocations
                // for each next location, get paths from nextLocation to toLocation
                |> List.fold (fun (paths: ((int*int) list) list) nextLocation ->
                        let pathsFound = fp visited' pathsAcc' nextLocation toLocation
                        pathsFound @ paths
                        ) []
        fp Set.empty [] startLocation endLocation
 
    let getTrailScore trailStart trailEnds data =
            trailEnds
            |> List.map (fun trailEnd ->
                let paths = findPaths trailStart trailEnd data
                List.length paths
                )
            |> List.sum
       
    let trailStarts = getTrailStarts data
    let trailEnds = getTrailEnds data |> Set.toList
    
    let scores =
        trailStarts
        |> Set.toList
        |> List.map (fun trailStart ->
                trailStart, getTrailScore trailStart trailEnds data            
            )
            
    let result =
        scores
        |> List.sumBy snd
    
    printfn $"Day 10 Part 2:- Result: %i{result}"
    
    ()

