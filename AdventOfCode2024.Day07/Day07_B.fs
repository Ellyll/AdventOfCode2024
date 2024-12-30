module Day07_B

open System
open System.Collections.Concurrent
open System.IO

let run () =
    printfn "Running Day07_B..."
    let filename = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day07_data.txt")
    
    let data =
        File.ReadAllLines(filename)
        |> Array.map (fun line ->
            match line.Split(':', StringSplitOptions.TrimEntries) with
            | [| tv ; nums |] -> int64 tv, nums.Split(' ', StringSplitOptions.TrimEntries) |> List.ofArray |> List.map int64
            | _ -> failwithf $"Unable to parse %s{line}"
            )
       
    let (|||) (a: int64) (b: int64) =
        int64 <| a.ToString() + b.ToString()
        
    // for each "line":
    //  generate all possible operator combinations for numbers.Length-1
    let availableOperators = [ (*) ; (+) ; (|||) ]
    let rec possibleOperators (n: int) : ('a list) list =
        let rec loop (collected: ('a list) list) remainingN =
            //printfn $"collected: %A{collected}, remainingN: %i{remainingN}"
            if remainingN = 0 then
                collected
            elif List.isEmpty collected then
                loop (availableOperators |> List.map (fun x -> [x]))  (remainingN-1)
            else
                let newCollected =
                    collected
                    |> Seq.fold (fun collected' col ->
                            // for each collected we need col @ [op] @ remaining
                            let newCols =
                                availableOperators
                                |> List.map (fun op ->
                                        col @ [op]
                                        //state @ (loop (col @ [op]) (remainingN-1))
                                    )
                            collected' @ newCols) []
                loop newCollected (remainingN-1)
                                    
        loop [] n

    let opsCache = ConcurrentDictionary<int, (int64 -> int64 -> int64) list list>()
    let rec cachedPossibleOperators n =
        if opsCache.ContainsKey n then
            let isSuccess,result = opsCache.TryGetValue n
            if not isSuccess then
                failwithf $"Error: failed to retrieve value for %i{n} from opsCache"
            else
                result
        else
            let result = possibleOperators n
            let isSuccess = opsCache.TryAdd(n, result)
            if not isSuccess then
                failwithf $"Error: failed to add value for %i{n} to opsCache"
            else
                result           
    
    let tryExecute (testValue: int64) (numbers: int64 list) (operators: (int64 -> int64 -> int64) list) : int64 option =
        let rec tryFold total nums ops =
            if total > testValue then
                None
            else
                match ops with
                | [] -> Some total
                | op::ops' ->
                    let total' = op total (nums |> List.head)
                    tryFold total' (nums |> List.tail) ops' 
        tryFold (numbers |> List.head) (numbers |> List.tail) operators

        
    let trueTestValues =
        data
        |> Array.filter (fun (testValue, numbers) ->
                let posOps = (cachedPossibleOperators (List.length numbers - 1)) |> Seq.toList
                posOps
                |> Seq.exists (fun operators ->
                    match tryExecute testValue numbers operators with
                    | Some total -> total = testValue
                    | None -> false
                    )
                )
    
    let result =
        trueTestValues
        |> Array.sumBy fst
    
    printfn $"Day 07 Part 2:- Result: %i{result}"
    
    ()
