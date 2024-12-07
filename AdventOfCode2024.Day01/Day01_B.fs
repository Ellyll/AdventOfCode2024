module Day01_B

open System
open System.IO

let run () =
    printfn "Running Day01_B..."
    let filename = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day01_data.txt")
    
    let data =
        File.ReadAllLines(filename)
        |> Array.map (fun line ->
            match line.Split("   ") with
            | [| a ; b |] -> (int a),(int b)
            | invalid -> failwithf $"Error parsing line: {invalid}"
            )
    
    let (list1,list2) =
        data
        |> Array.fold (fun (l1,l2) (a,b) -> a::l1, b::l2) ([],[]) // split

    let counted =
        list2
        |> List.countBy id
        |> Map.ofList
    
    let result =
        list1
        |> List.fold (fun total a ->
            let occurrences = counted |> Map.tryFind a |> Option.defaultValue 0
            total + (a * occurrences)           
            ) 0
    
    printfn $"Day 01 Part 2:- Result: %i{result}"
    
    () 

