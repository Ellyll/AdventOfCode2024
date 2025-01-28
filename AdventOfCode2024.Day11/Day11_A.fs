module Day11_A

open System
open System.IO

let run () =
    printfn "Running Day11_A..."
    
    let filename = Path.Combine(__SOURCE_DIRECTORY__, "Data", "Day11_data.txt")
    let stones =
        File.ReadAllText(filename).Trim().Split(" ")
        |> Array.map int64
        |> Array.toList
       
    let applyRules (stones: int64 list) =
        stones
        |> List.fold (fun newStones stone ->
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
                1L::newStones
            elif stone.ToString().Length % 2 = 0 then
                let stoneStr : string = stone.ToString()
                let len = stoneStr.Length
                let newStone1 = stoneStr[0..(len/2)-1] |> int64
                let newStone2 = stoneStr[(len/2)..] |> int64
                newStone2::newStone1::newStones
            else
                (stone*2024L)::newStones
            ) []

    let blinkedStones =
        seq { 1..25 }
        |> Seq.fold (fun newStones _ ->
            applyRules newStones
            ) stones
    
    let result =
        blinkedStones
        |> List.length
    
    printfn $"Day 11 Part 1:- Result: %i{result}"
    
    ()

