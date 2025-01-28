module Day11_B

open System
open System.IO

let run () =
    printfn "Running Day11_B..."

    let filename = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day11_data.txt")
    let stones =
        File.ReadAllText(filename).Trim().Split(" ")
        |> Array.map int64
        |> Array.fold (fun stonesMap stone ->
            stonesMap
            |> Map.change stone (function
                | None -> Some 1L
                | Some n -> Some (n + 1L)
                )
            ) Map.empty

    let applyRules (stones: Map<int64,int64>) =
        stones
        |> Map.fold (fun newStones stone count ->
            (*            
                - If the stone is engraved with the number 0, it is replaced by a stone engraved
                  with the number 1.
                - If the stone is engraved with a number that has an even number of digits, it is
                  replaced by two stones. The left half of the digits are engraved on the new left
                  stone, and the right half of the digits are engraved on the new right stone.
                  (The new numbers don't keep extra leading zeroes: 1000 would become stones 10
                  and 0.)
                - If none of the other rules apply, the stone is replaced by a new stone; the old
                  stone's number multiplied by 2024 is engraved on the new stone.
            *)
            if stone = 0L then
                newStones
                |> Map.change 1L (function
                    | None -> Some count
                    | Some cnt -> Some (cnt + count)
                    )
            elif stone.ToString().Length % 2 = 0 then
                let stoneStr : string = stone.ToString()
                let len = stoneStr.Length
                let newStone1 = stoneStr[0..(len/2)-1] |> int64
                let newStone2 = stoneStr[(len/2)..] |> int64
                newStones
                |> Map.change newStone1 (function
                    | None -> Some count
                    | Some cnt -> Some (cnt + count)
                    )
                |> Map.change newStone2 (function
                    | None -> Some count
                    | Some cnt -> Some (cnt + count)
                    )
            else
                let newStone = stone * 2024L
                newStones
                |> Map.change newStone (function
                    | None -> Some count
                    | Some cnt -> Some (cnt + count)
                    )
            ) Map.empty

    let blinkedStones =
        seq { 1..75 }
        |> Seq.fold (fun newStones i ->
            applyRules newStones
            ) stones

    let result =
        blinkedStones
        |> Map.fold (fun total _ count -> total + count) 0L
    
    printfn $"Day 11 Part 2:- Result: %i{result}"
    
    ()

