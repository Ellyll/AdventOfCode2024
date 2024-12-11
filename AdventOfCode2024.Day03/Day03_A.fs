module Day03_A

open System.IO
open System.Text.RegularExpressions

let run () =
    printfn "Running Day03_A..."
    let filename = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day03_data.txt")
    
    let regMul = Regex(@"mul\((\d{1,3}),(\d{1,3})\)")
    
    let data =
        File.ReadAllText(filename)
 
    let result =
        regMul.Matches(data)
        |> Seq.map (fun m -> (int m.Groups[1].Value) * (int m.Groups[2].Value))
        |> Seq.sum

    printfn $"Day 03 Part 1:- Result: %i{result}"
    
    () 

