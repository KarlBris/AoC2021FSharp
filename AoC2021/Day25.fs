namespace AoC2021

open Utils

module Day25 =

    type SeaFloor =
        | Empty
        | East
        | South

    let parseChar (c: char) : SeaFloor =
        match c with
        | '>' -> East
        | 'v' -> South
        | _ -> Empty

    let parseLine (line: string) : SeaFloor [] =
        line |> Array.ofSeq |> Array.map parseChar

    let arrayGetWrap (i: int) (j: int) (floor: SeaFloor [,]) : SeaFloor =
        let length1 = Array2D.length1 floor
        let length2 = Array2D.length2 floor
        let newI = eMod i length1
        let newJ = eMod j length2

        Array2D.get floor newI newJ

    let stepSouth (i: int) (j: int) (f: SeaFloor) (floor: SeaFloor [,]) : SeaFloor =
        // i goes down, j goes right
        match f with
        | South -> // if the one below is empty, set this to empty, otherwise stay South
            if arrayGetWrap (i + 1) j floor = Empty then
                Empty
            else
                South
        | East -> East
        | Empty -> // If the one above is South, set this to South, otherwise stay Empty
            if arrayGetWrap (i - 1) j floor = South then
                South
            else
                Empty

    let stepEast (i: int) (j: int) (f: SeaFloor) (floor: SeaFloor [,]) : SeaFloor =
        // i goes down, j goes right
        match f with
        | South -> South
        | East -> // if the one to the right is empty, set this to empty, otherwise stay East
            if arrayGetWrap i (j + 1) floor = Empty then
                Empty
            else
                East
        | Empty -> // If the one to the left is East, set this to East, otherwise stay Empty
            if arrayGetWrap i (j - 1) floor = East then
                East
            else
                Empty

    let rec step (count: int) (floor: SeaFloor [,]) : int =
        let floorStepEast =
            floor
            |> Array2D.mapi (fun i j v -> stepEast i j v floor)

        let floorStepSouth =
            floorStepEast
            |> Array2D.mapi (fun i j v -> stepSouth i j v floorStepEast)

        if floorStepSouth = floor then
            count
        else
            step (count + 1) floorStepSouth

    let part1 (input: string) : string =
        let foo =
            input |> lines |> Array.map parseLine |> array2D

        let count = step 1 foo

        count |> string

    let part2 (input: string) : string = input
