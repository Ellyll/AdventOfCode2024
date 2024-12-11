module Day03_B

open System
open System.IO
open System.Text.RegularExpressions

type Intruction =
        | Mul of int*int
        | Enable
        | Disable

let run () =
    printfn "Running Day03_B..."
    let filename = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day03_data.txt")
    
    let regMul = Regex(@"(mul\((\d{1,3}),(\d{1,3})\))|(do\(\))|(don't\(\))")
    
    let data = File.ReadAllText(filename)
 
    let program =
        regMul.Matches(data)
        |> Seq.map (fun m ->
            if not <| String.IsNullOrEmpty(m.Groups[1].Value) then
                // Is Mul
                Mul ((int m.Groups[2].Value), (int m.Groups[3].Value))
            elif not <| String.IsNullOrEmpty(m.Groups[4].Value) then
                Enable
            elif not <| String.IsNullOrEmpty(m.Groups[5].Value) then
                Disable
            else
                failwithf $"Unable to parse %A{m}"               
            )

    let _, result =
        program
        |> Seq.fold (fun (isEnabled, total) instruction ->
            match instruction with
            | Mul (a,b) ->
                if isEnabled then
                    isEnabled, total + (a * b)
                else
                    isEnabled, total
            | Enable -> true, total
            | Disable -> false, total
            ) (true,0)
    
    printfn $"Day 03 Part 2:- Result: %i{result}"
    
    () 

