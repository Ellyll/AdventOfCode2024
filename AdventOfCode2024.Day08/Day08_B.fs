module Day08_B

open System
open System.IO
open System.Text.RegularExpressions

let run () =
    printfn "Running Day08_B..."
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
    
    let isOnALine (l1,c1) (l2,c2) (l,c) =
        // from https://stackoverflow.com/a/11908158
        // dxc = currPoint.x - point1.x;
        // dyc = currPoint.y - point1.y;
        // dxl = point2.x - point1.x;
        // dyl = point2.y - point1.y;
        // cross = dxc * dyl - dyc * dxl;
        // is on a line only if cross == 0
        let dxc = c - c1
        let dyc = l - l1
        let dxl = c2 - c1
        let dyl = l2 - l1
        let cross = dxc * dyl - dyc * dxl
        cross = 0
    
    let isAntinode gridPosition antennaPositions =
        let pairs = getAllPairs antennaPositions
        pairs
        |> List.exists (fun pair ->
                match pair with
                | [ p1 ; p2 ] -> isOnALine p1 p2 gridPosition
                | _ -> failwithf $"Not a pair! %A{pair}"
            )
    
    let antinodes =
        seq {
            for l in 0..(height-1) do
                for c in 0..(width-1) -> (l,c)
        }
        |> Seq.filter (fun gridPosition ->
            // for each grid position check if it exists on a line of one of the pairs of antenna
            data
            |> Map.keys
            |> Seq.exists (fun key ->
                let antennaPositions = data |> Map.find key
                isAntinode gridPosition antennaPositions
                )
            )

    let result =
        antinodes
        |> Seq.length       
    
    printfn $"Day 08 Part 2:- Result: %i{result}"
    
    ()

