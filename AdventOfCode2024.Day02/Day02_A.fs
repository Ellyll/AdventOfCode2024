module Day02_A

open System.IO

let run () =
    printfn "Running Day02_A..."
    let filename = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day02_data.txt")
    
    let data =
        File.ReadAllLines(filename)
        |> Array.map (fun line ->
            line.Split(" ") |> Array.map int |> Array.toList
            )
           
    let isSafe (readings: int list) =
        let rec loop (remaining: int list) (prev: int option) (direction: int option) : bool =
            match remaining, prev, direction with
            | [], _, _ -> true // Finished going through list
            | x::xs, None, None -> loop xs (Some x) None // First time - no previous to compare with
            | x::xs, (Some p), None -> // Second time - no direction to compare with
                let dir = if x = p then 0 elif x > p then 1 else -1
                if dir = 0 then
                    false
                else
                    let diff = abs (x - p)
                    if diff < 1 || diff > 3 then
                        false
                    else
                        loop xs (Some x) (Some dir) 
            | x::xs, (Some p), (Some prevDir) ->
                let dir = if x = p then 0 elif x > p then 1 else -1
                if dir = 0 || dir <> prevDir then
                    false
                else
                    let diff = abs (x - p)
                    if diff < 1 || diff > 3 then
                        false
                    else
                        loop xs (Some x) (Some dir)
            | _ ->
                failwithf $"Something wierd happened: %A{remaining} %A{prev} %A{direction}"
        loop readings None None
     
                            
    let safeReports =
        data
        |> Seq.filter isSafe
                          
    let result = safeReports |> Seq.length 
    
    printfn $"Day 02 Part 1:- Result: %i{result}"
    
    () 

