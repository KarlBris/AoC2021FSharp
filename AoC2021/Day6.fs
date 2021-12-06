namespace AoC2021

open Utils

module Day6 =

    let makeFishMap (fishList: int []) : Map<int, int64> =
        Array.fold
            (fun (fishList: Map<int, int64>) (fish: int) ->
                Map.change
                    fish
                    (fun numFishes ->
                        match numFishes with
                        | Some num -> Some(num + 1L)
                        | None -> Some 1L)
                    fishList)
            (Map.ofArray (Array.zip [| 0 .. 8 |] (Array.replicate 9 0L)))
            fishList

    let spawnGeneration (fishList: int64 seq) : (int * int64) seq =
        let fishToSpawn = Seq.head fishList
        let restOfFish = Seq.tail fishList

        Seq.append restOfFish [ fishToSpawn ]
        |> Seq.mapi
            (fun i fishNums ->
                if i = 6 then
                    fishNums + fishToSpawn
                else
                    fishNums)
        |> Seq.zip [ 0 .. 8 ]

    let rec spawnFish (daysLeft: int) (fishMap: Map<int, int64>) : int64 =
        match daysLeft with
        | 0 -> Map.values fishMap |> Seq.sum
        | _ ->
            spawnFish
                (daysLeft - 1)
                (fishMap
                 |> Map.values
                 |> spawnGeneration
                 |> Map.ofSeq)

    let part1 (input: string) : string =
        input
        |> commas
        |> Array.map int
        |> makeFishMap
        |> spawnFish 80
        |> string

    let part2 (input: string) : string =
        input
        |> commas
        |> Array.map int
        |> makeFishMap
        |> spawnFish 256
        |> string
