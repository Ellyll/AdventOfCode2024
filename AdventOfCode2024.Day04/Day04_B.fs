module Day04_B

open System.IO

let run () =
    printfn "Running Day04_B..."
    let filename = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day04_data.txt")
    
   
    let lines = File.ReadAllLines(filename)
    let width = lines[0] |> String.length
    let height = lines |> Array.length

    let data =
        Array2D.init height width (fun r c -> lines[r][c])

    let foldi2D folder initialState arr =
        let mutable state = initialState
        for r in 0 .. (Array2D.length1 arr)-1 do
            for c in 0 .. (Array2D.length2 arr)-1 do
                state <- folder state r c arr[r,c]
        state

    let findXMasAt r c arr =       
        // Bounds check, and is it an A? and is it an X-Mas?
        if r-1 >= 0 && r+1 < (Array2D.length1 arr)
            && c-1 >= 0 && c+1 < (Array2D.length2 arr)
            && arr[r,c] = 'A' then
                let word1 = System.String [| arr[r-1,c-1] ; 'A' ; arr[r+1,c+1] |]
                let word2 = System.String [| arr[r-1,c+1] ; 'A' ; arr[r+1,c-1] |]
                (word1 = "MAS" || word1 = "SAM")
                   && (word2 = "MAS" || word2 = "SAM")
        else
            false
        
    let found =
        data
        |> foldi2D (fun state r c value ->
            match findXMasAt r c data with
            | false -> state
            | true -> [r,c]::state
            ) []
    
    let result =
        found
        |> List.length
    
    printfn $"Day 04 Part 2:- Result: %i{result}"
    
    () 

