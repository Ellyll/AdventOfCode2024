module Day08_A

open System
open System.IO
open System.Text.RegularExpressions

let run () =
    printfn "Running Day08_A..."
    let filename = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day08_data.txt")
    
    let lines = File.ReadAllLines(filename)
    let width = lines[0] |> String.length
    let height = lines |> Array.length
    
    let data =
        let regFreq = Regex(@"^[A-Za-z0-9]$")
        seq {
            for l in 0..(height-1) do
                for c in 0..(width-1) -> l,c
        }
        |> Seq.fold (fun freqMap (l,c) ->
            let frequency = lines[l][c..c]
            if regFreq.IsMatch(frequency) then                
                freqMap
                |> Map.change frequency (function
                    | Some positions -> positions |> Set.add (l,c) |> Some
                    | None -> Set.singleton (l,c) |> Some)
            else
                freqMap
            ) Map.empty

    
    let getAllPairs (set: Set<'a>) =
        // from https://stackoverflow.com/a/1228715
        let combHelper el lst =
            lst |> List.map (fun lstEl -> el::[lstEl])
        let filterOut el lst =
            lst |> List.filter (fun lstEl -> lstEl <> el)
        let lst = set |> Set.toList
        lst |> List.map (fun lstEl -> combHelper lstEl (filterOut lstEl lst)) |> List.concat

    let getAntinodes freqPositions =
        let pairs = getAllPairs freqPositions
        pairs
        |> List.map (fun positions ->
            match positions with
            | [ (l1,c1) ; (l2,c2) ] ->
                let ld,cd = l1-l2, c1-c2
                let antinode = l1+ld, c1+cd
                antinode
            | _ -> failwithf $"Error: positions was not a pair %A{positions}"
            )
        
    let antinodes =
        data
        |> Map.fold (fun anodes _ positions ->
            getAntinodes positions @ anodes) []
        |> List.filter (fun (l,c) -> l >= 0 && l < height && c >= 0 && c < width)
        |> Set.ofList
      
    let result =
        antinodes
        |> Set.count
    
    printfn $"Day 08 Part 1:- Result: %i{result}"
    
    ()

