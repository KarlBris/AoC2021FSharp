namespace AoC2021

open Utils

module Day11 =

    type Grid = int [,]

    let makeOctopusGrid (input: string) : Grid =
        input
        |> lines
        |> Array.map Array.ofSeq
        |> Array.map (Array.map string)
        |> Array.map (Array.map int)
        |> array2D

    let oneStep (grid: Grid) : Grid = Array2D.map (fun o -> o + 1) grid

    let adjacentPoints (pos: int * int) : (int * int) [] =
        let (x, y) = pos

        [| for x2 in [| (x - 1) .. (x + 1) |] do
               for y2 in [| (y - 1) .. (y + 1) |] do
                   if x2 >= 0 && x2 <= 9 && y2 >= 0 && y2 <= 9 then
                       yield (x2, y2) |]

    let applyFlash (flashMap: Map<(int * int), int>) (flashPos: int * int) : Map<(int * int), int> =
        adjacentPoints flashPos
        |> Array.fold
            (fun state adjPos ->
                Map.change
                    adjPos
                    (fun o ->
                        match o with
                        | Some v -> Some(v + 1)
                        | _ -> Some 1)
                    state)
            flashMap

    let resetFlashes (grid: Grid) : Grid =
        grid
        |> Array2D.map (fun v -> if v > 9 then 0 else v)

    let rec resolveFlashes (visiteds: (int * int) []) (grid: Grid) : (int * Grid) =
        let flashPositions =
            Array2D.mapi (fun x y o -> if o > 9 then (x, y) else (-10, -10)) grid

        let flashList =
            flashPositions
            |> Seq.cast<int * int>
            |> Seq.filter (fun p -> p <> (-10, -10) && not (Array.contains p visiteds))
            |> Array.ofSeq

        match flashList with
        | [||] -> (visiteds.Length, resetFlashes grid)
        | _ ->
            let allFlashSum =
                Array.fold applyFlash Map.empty flashList

            allFlashSum
            |> Map.toArray
            |> Array.iter (fun ((x, y), v) -> Array2D.set grid x y ((Array2D.get grid x y) + v))

            let newGrid = grid
            resolveFlashes (Array.append visiteds flashList) (newGrid)

    let rec runSimulation (steps: int) (acc: int) (grid: Grid) : int =
        match steps with
        | 0 -> acc
        | _ ->
            let (newAcc, newGrid) = oneStep grid |> resolveFlashes [||]
            runSimulation (steps - 1) (acc + newAcc) newGrid

    let isAllZero (grid: Grid) : bool =
        grid
        |> Seq.cast<int>
        |> Array.ofSeq
        |> Array.forall (fun v -> v = 0)

    let rec runSimulationIndefinitely (steps: int) (grid: Grid) : int =
        let (flashes, newGrid) = oneStep grid |> resolveFlashes [||]

        if flashes = 100 then
            (steps + 1)
        else
            runSimulationIndefinitely (steps + 1) newGrid

    let part1 (input: string) : string =
        input
        |> makeOctopusGrid
        |> runSimulation 100 0
        |> string

    let part2 (input: string) : string =
        input
        |> makeOctopusGrid
        |> runSimulationIndefinitely 0
        |> string
