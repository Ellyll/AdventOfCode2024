module Day01_A

open System
open System.IO

let run () =
    printfn "Running Day01_A..."
    let filename = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day01_data.txt")
    
    let data =
        File.ReadAllLines(filename)
        |> Array.map (fun line ->
            match line.Split("   ") with
            | [| a ; b |] -> (int a),(int b)
            | invalid -> failwithf $"Error parsing line: {invalid}"
            )
    
    // split, sort and recombine lists
    let sortedData =
        data
        |> Array.fold (fun (l1,l2) (a,b) -> a::l1, b::l2) ([],[]) // split
        |> fun (l1,l2) -> (l1 |> List.sort), (l2 |> List.sort) // sort
        |> fun (l1,l2) -> l1 |> List.zip l2 // recombine
    
    let distances =
        sortedData        
        |> List.map (fun (a,b) -> abs (a - b))
                          
    let result = distances |> List.sum
    
    printfn $"Day 01 Part 1:- Result: %i{result}"
    
    () 

