module Day04_A

open System.IO

let run () =
    printfn "Running Day04_A..."
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

    let findWordAt (word: string) r c arr =
        // N, NE, E, SE, S, SW, W, NW
        let directions = [|
            1,0
            1,1
            0,1
            -1,1
            -1,0
            -1,-1
            0,-1
            1,-1
        |]
        let rec searchDirection direction remaining rCurr cCurr =
            match remaining with
            | [] -> true
            | x::xs ->
                // in range?
                if rCurr >= 0 && rCurr < (Array2D.length1 arr)
                    && cCurr >= 0 && cCurr < (Array2D.length2 arr) then
                        // Check current position
                        if arr[rCurr,cCurr] = x then
                            let rCurr' = rCurr + fst direction
                            let cCurr' = cCurr + snd direction
                            searchDirection direction xs rCurr' cCurr'
                        else
                            false
                else
                    false
            
        let wordChars = (word.ToCharArray() |> Array.toList )
        directions
        |> Array.fold (fun found dir ->
            if searchDirection dir wordChars r c then
                (r,c,dir)::found
            else
                found
            ) []
            
    let found =
        data
        |> foldi2D (fun state r c value ->
            match findWordAt "XMAS" r c data with
            | [] -> state
            | found -> found @ state
            ) []
    
    let result =
        found
        |> List.length

    printfn $"Day 04 Part 1:- Result: %i{result}"
    
    () 

