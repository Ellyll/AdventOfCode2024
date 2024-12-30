module Day07_A

open System
open System.IO

let run () =
    printfn "Running Day07_A..."
    let filename = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day07_data.txt")
    
    let data =
        File.ReadAllLines(filename)
        |> Array.map (fun line ->
            match line.Split(':', StringSplitOptions.TrimEntries) with
            | [| tv ; nums |] -> int64 tv, nums.Split(' ', StringSplitOptions.TrimEntries) |> List.ofArray |> List.map int64
            | _ -> failwithf $"Unable to parse %s{line}"
            )
        
    // for each "line":
    //  generate all possible operator combinations for numbers.Length-1
    let possibleOperators (n: int) : ((int64 -> int64 -> int64) seq) seq =
        // only two possible operators so use binary numbers to get combinations
        // convert 0s to * and 1s to +
        seq { 0..((pown 2 n)-1) }
        |> Seq.map (fun i ->
            seq { (n-1) .. -1 .. 0 }
            |> Seq.map (fun pos ->
                let mask = 1 <<< pos // check bit at position pos
                if i &&& mask = 0 then (*) else (+)
                )
            )

    let rec permutations list =
        let insertions x xs =
            [ for i in 0..List.length xs do
                yield List.take i xs @ [x] @ List.skip i xs ]
        match list with
        | [] -> [[]]
        | x::xs -> List.collect (insertions x) (permutations xs)
    
    let execute (numbers: int64 list) (operators: (int64 -> int64 -> int64) seq) : int64 =
        operators
        |> Seq.fold (fun (total,nums) op ->
            // a op1 b op2 c op3 d
            // (((a op1 b) op2 c) op3 d)
            (op total (nums |> List.head)), nums |> List.tail 
            ) (numbers |> List.head, numbers |> List.tail)
        |> fst
        
    let trueTestValues =
        data
        |> Array.filter (fun (testValue, numbers) ->
                let posOps = (possibleOperators (List.length numbers - 1)) |> Seq.toList
                posOps
                |> Seq.exists (fun operators ->
                    let total = execute numbers operators
                    total = testValue
                    )
                )
    
    let result =
        trueTestValues
        |> Array.sumBy fst
    
    printfn $"Day 07 Part 1:- Result: %i{result}"
    
    ()
