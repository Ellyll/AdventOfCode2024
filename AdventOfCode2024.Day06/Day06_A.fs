module Day06_A

open System
open System.IO

type Direction =
        | Up
        | Down
        | Left
        | Right

type Guard =
    {
        Position: int*int
        Facing: Direction
    }


let run () =
    printfn "Running Day06_A..."
    let filename = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day06_data.txt")
    
    let lines = File.ReadAllLines(filename)
    let width = lines[0] |> String.length
    let height = lines |> Array.length
    
    let _, obstructions, guardOption =
        lines
        |> Array.fold (fun (l,obstructions,guardOption) line ->
            let (_, obstructions', guardOption') =
                line
                |> Seq.fold (fun (c,obs, guardOpt) chr ->
                    match chr with
                    | '.' -> c+1, obs, guardOpt
                    | '#' -> c+1, obs |> Set.add (l,c), guardOpt
                    | '^' -> c+1, obs, Some ({ Position = (l,c) ; Facing = Up })
                    | _ -> failwithf $"Unable to parse character %c{chr} at (%i{l},%i{c})"
                    ) (0,obstructions, guardOption)
            l+1, obstructions', guardOption'
            ) (0,Set.empty, None)
   
    let guard =
        match guardOption with
        | Some p -> p
        | None -> failwith "Guard position not found"
    
    let getNextPosition guard =
        let l,c = guard.Position
        match guard.Facing with
        | Up -> l-1, c
        | Down -> l+1, c
        | Left -> l, c-1
        | Right -> l, c+1
        
    let turnRight guard =
        let facing =
            match guard.Facing with
            | Up -> Right
            | Right -> Down
            | Down -> Left
            | Left -> Up
        { guard with Facing = facing }
    
    let positionsVisited =
        let rec loop visited guard =
            let l,c = guard.Position
            if l < 0 || c < 0 || l >= height || c >= width then
               visited
            else
                let visited' = visited |> Set.add guard.Position
                let nextPosition = getNextPosition guard
                if obstructions |> Set.contains nextPosition then
                    loop visited' (guard |> turnRight)
                else
                    loop visited' { guard with Position = nextPosition }
                
        loop Set.empty guard

    let result =
        positionsVisited
        |> Set.count
    
    printfn $"Day 06 Part 1:- Result: %i{result}"
    
    ()

