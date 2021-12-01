namespace AoC2021

open TestRunner

module Day1 =

    let isIncreasing ((l, r): int * int) : int = if r > l then 1 else 0

    let part1 (input: string) : string =
        input.Split "\n"
        |> Array.map int
        |> Array.pairwise
        |> Array.map isIncreasing
        |> Array.sum
        |> string

    let part2 (input: string) : string =
        input.Split "\n"
        |> Array.map int
        |> Array.windowed 3
        |> Array.map Array.sum
        |> Array.pairwise
        |> Array.map isIncreasing
        |> Array.sum
        |> string
