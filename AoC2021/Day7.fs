namespace AoC2021

open Utils

module Day7 =

    let part1 (input: string) : string =
        let inputs = input |> commas |> Array.map int
        let maxdepth = Array.max inputs

        [| 0 .. maxdepth |]
        |> Array.map (fun d -> inputs |> Array.map (fun i -> abs (i - d)))
        |> Array.map Array.sum
        |> Array.min
        |> string

    let part2 (input: string) : string =
        let inputs = input |> commas |> Array.map int
        let maxdepth = Array.max inputs

        let sum = (inputs |> Array.sum |> float)

        let avg =
            int (((sum) / (float (inputs.Length))) + 0.1)


        inputs
        |> Array.map (fun i -> ((((abs (i - avg)) * ((abs (i - avg)) + 1)) / 2)))
        |> Array.sum
        |> string
