namespace AoC2021

open Utils
open System

module Day15 =

    type Pos = int * int

    let makeGrid input =
        let ls =
            input
            |> lines
            |> Array.map (Array.ofSeq)
            |> Array.map (Array.map (fun c -> (int c) - (int '0')))

        let ySize = ls.Length
        let xSize = ls.[0].Length
        (xSize, ySize, array2D ls)

    let rec increaseAndWrap amount n =
        match amount with
        | 0 -> n
        | _ ->
            match n with
            | 9 -> increaseAndWrap (amount - 1) 1
            | _ -> increaseAndWrap (amount - 1) (n + 1)

    let manhattanDist ((x1, y1): Pos) ((x2, y2): Pos) : int = (abs (x1 - x2)) + (abs (y1 - y2))

    let neighbors ((x, y): Pos) (multiplier: int) (grid: int [,]) : Pos [] =
        let xSize = Array2D.length1 grid * multiplier
        let ySize = Array2D.length2 grid * multiplier

        [| (x + 1, y)
           (x - 1, y)
           (x, y + 1)
           (x, y - 1) |]
        |> Array.filter (fun (x, y) -> x >= 0 && y >= 0 && x < xSize && y < ySize)

    let distance (_current: Pos) (neighbor: Pos) (multiplier: int) (grid: int [,]) : int =
        let (x, y) = neighbor
        let xSize = Array2D.length1 grid * multiplier
        let ySize = Array2D.length2 grid * multiplier
        let realxSize = Array2D.length1 grid
        let realySize = Array2D.length2 grid
        let xDiv = x / realxSize
        let yDiv = y / realySize
        let increases = manhattanDist (xDiv, yDiv) (0, 0)

        if x < 0 || y < 0 || x >= xSize || y >= ySize then
            Int32.MaxValue
        else
            increaseAndWrap grid.[(x % realxSize), (y % realySize)] increases

    let rec pathToPos (cameFrom: Map<Pos, Pos>) (acc: Pos list) : Pos [] =
        let current = List.head acc

        match Map.tryFind current cameFrom with
        | Some v -> pathToPos cameFrom (v :: acc)
        | None -> Array.ofList acc

    let findOrMax k map =
        match Map.tryFind k map with
        | Some v -> v
        | None -> Int32.MaxValue

    let pathScore (grid: int [,]) (path: Pos []) : int =
        let xSize = Array2D.length1 grid
        let ySize = Array2D.length2 grid

        Array.tail path
        |> Array.map
            (fun (x, y) ->
                let xDiv = x / xSize
                let yDiv = y / ySize
                let increases = manhattanDist (xDiv, yDiv) (0, 0)
                increaseAndWrap grid.[x % xSize, y % ySize] increases)
        |> Array.sum

    let rec checkNeighbors neighbors current gScore multiplier grid cameFrom fScore openSet h goal =
        match neighbors with
        | [||] -> (cameFrom, gScore, fScore, openSet)
        | _ ->
            let n = Array.head neighbors

            let tentativeGScore =
                (Map.find current gScore)
                + (distance current n multiplier grid)

            if tentativeGScore < findOrMax n gScore then
                let cameFrom2 = Map.add n current cameFrom
                let gScore2 = Map.add n tentativeGScore gScore

                let fScore2 =
                    Map.add n (tentativeGScore + (h n goal)) fScore

                let openSet2 = Set.add n openSet
                checkNeighbors (Array.tail neighbors) current gScore2 multiplier grid cameFrom2 fScore2 openSet2 h goal
            else
                checkNeighbors (Array.tail neighbors) current gScore multiplier grid cameFrom fScore openSet h goal

    let rec aStarRunner (openSet: Set<Pos>) fScore gScore cameFrom goal multiplier h grid =
        match Set.isEmpty openSet with
        | true -> failwith "error"
        | false ->
            let currentPos =
                Set.toArray openSet
                |> Array.sortBy (fun p -> findOrMax p fScore)
                |> Array.head

            if currentPos = goal then
                pathToPos cameFrom [ currentPos ]
                |> pathScore grid
            else
                let openSet2 = Set.remove currentPos openSet
                let ns = (neighbors currentPos multiplier grid)

                let (cameFrom2, gScore2, fScore2, openSet3) =
                    checkNeighbors ns currentPos gScore multiplier grid cameFrom fScore openSet2 h goal

                aStarRunner openSet3 fScore2 gScore2 cameFrom2 goal multiplier h grid

    let aStar (start: Pos) (goal: Pos) (grid: int [,]) (h: Pos -> Pos -> int) (multiplier: int) =
        let openSet = set [| start |]
        let cameFrom: Map<Pos, Pos> = Map.empty

        let gScore = Map.ofArray [| (start, 0) |]
        let fScore = Map.ofArray [| (start, h start goal) |]

        aStarRunner openSet fScore gScore cameFrom goal multiplier manhattanDist grid

    let part1 (input: string) : string =
        let (xSize, ySize, grid) = input |> makeGrid

        aStar (0, 0) (xSize - 1, ySize - 1) grid manhattanDist 1
        |> string

    let part2 (input: string) : string =
        let (xSize, ySize, grid) = input |> makeGrid

        aStar (0, 0) (xSize * 5 - 1, ySize * 5 - 1) grid manhattanDist 5
        |> string
