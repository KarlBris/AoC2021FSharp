namespace AoC2021

open Utils
open FSharp.Collections

module Day5 =

    type OverlapMap = Map<(int * int), int>
    type Line = ((int * int) * (int * int))

    let makePoint (pointPair: string) : (int * int) =
        let points = pointPair.Split ","
        (int points.[0], int points.[1])

    let processInput (line: string) : Line =
        line.Split " -> "
        |> Array.map makePoint
        |> fun ps -> (ps.[0], ps.[1])

    let makeLinePoints (line: Line) : (int * int) [] =
        let ((x1, y1), (x2, y2)) = line

        if x1 = x2 then
            let ys =
                if y1 < y2 then
                    [| y1 .. y2 |]
                else
                    Array.rev [| y2 .. y1 |]

            let xs = Array.replicate ys.Length x1
            Array.zip xs ys
        else if y1 = y2 then
            let xs =
                if x1 < x2 then
                    [| x1 .. x2 |]
                else
                    Array.rev [| x2 .. x1 |]

            let ys = Array.replicate xs.Length y1
            Array.zip xs ys
        else
            let xs =
                if x1 < x2 then
                    [| x1 .. x2 |]
                else
                    Array.rev [| x2 .. x1 |]

            let ys =
                if y1 < y2 then
                    [| y1 .. y2 |]
                else
                    Array.rev [| y2 .. y1 |]

            Array.zip xs ys

    let isStraight (line: Line) : bool =
        let ((x1, y1), (x2, y2)) = line

        x1 = x2 || y1 = y2

    let isStraightOrDiagonal (line: Line) : bool =
        let ((x1, y1), (x2, y2)) = line

        isStraight line || abs (x1 - x2) = abs (y1 - y2)

    let rec compareLines (map: OverlapMap) (diags: bool) (lines: Line []) : OverlapMap =
        match lines with
        | [||] -> map
        | _ ->
            let isValidLine =
                if diags then
                    isStraightOrDiagonal
                else
                    isStraight

            let firstLine = Array.head lines
            let restOfLines = Array.tail lines

            let newMap =
                if isValidLine firstLine then
                    Array.fold
                        (fun m ov ->
                            Map.change
                                ov
                                (fun x ->
                                    match x with
                                    | Some i -> Some(i + 1)
                                    | None -> Some 1)
                                m)
                        map
                        (makeLinePoints firstLine)
                else
                    map


            compareLines newMap diags restOfLines

    let countOverlaps (map: OverlapMap) : int =
        map
        |> Map.values
        |> Seq.filter (fun i -> i >= 2)
        |> Seq.length

    let part1 (input: string) : string =
        input
        |> lines
        |> Array.map processInput
        |> compareLines Map.empty false
        |> countOverlaps
        |> string

    let part2 (input: string) : string =
        input
        |> lines
        |> Array.map processInput
        |> compareLines Map.empty true
        |> countOverlaps
        |> string
