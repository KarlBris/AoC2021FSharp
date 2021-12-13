namespace AoC2021

open Utils

module Day13 =

    type Paper = Map<int * int, bool>

    type FoldAxis =
        | X of int
        | Y of int

    let markPaper (coords: string []) : Paper =
        coords
        |> Array.map commas
        |> Array.map (Array.map int)
        |> Array.map twoArrayToTuple
        |> Array.map (fun p -> p, true)
        |> Map.ofArray

    let parseFold (fold: string) : FoldAxis =
        let (_, e) =
            fold.Split("fold along ") |> twoArrayToTuple

        let (axis, loc) = e.Split("=") |> twoArrayToTuple

        match axis with
        | "x" -> X(int loc)
        | "y" -> Y(int loc)
        | _ -> failwith "fold fail!"

    let transformCoord (fold: FoldAxis) (coord: ((int * int) * bool)) : ((int * int) * bool) =
        let ((x, y), b) = coord

        match fold with
        | X a ->
            if x > a then
                ((a - (x - a), y), b)
            else
                ((x, y), b)
        | Y a ->
            if y > a then
                ((x, a - (y - a)), b)
            else
                ((x, y), b)

    let foldPaper (fold: FoldAxis) (paper: Paper) : Paper =
        Map.toArray paper
        |> Array.map (transformCoord fold)
        |> Map.ofArray

    let part1 (input: string) : string =
        let (coords, foldStrings) = input.Split("\n\n") |> twoArrayToTuple

        let folds =
            foldStrings |> lines |> Array.map parseFold

        let foldedPaper =
            coords
            |> lines
            |> markPaper
            |> foldPaper (Array.head folds)

        foldedPaper.Count |> string

    let showPaper (keys: (int * int) []) : unit =
        let (xs, ys) = Array.unzip keys

        for y in 0 .. (Array.max ys) do
            for x in 0 .. (Array.max xs) do
                if Array.contains (x, y) keys then
                    printf "#"
                else
                    printf " "

            printf "\n"

    let part2 (input: string) : string =
        let (coords, folds) = input.Split("\n\n") |> twoArrayToTuple
        let paper = coords |> lines |> markPaper

        folds
        |> lines
        |> Array.map parseFold
        |> Array.fold (fun p f -> foldPaper f p) paper
        |> Map.keys
        |> Array.ofSeq
        |> showPaper

        ""
