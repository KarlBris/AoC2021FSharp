namespace AoC2021

open Utils

module Day3 =

    let rec transpose =
        function
        | (_ :: _) :: _ as M ->
            List.map List.head M
            :: transpose (List.map List.tail M)
        | _ -> []

    let mostCommonBit (chars: char list) =
        let (ones, zeroes) = List.partition (fun c -> c = '1') chars

        if ones.Length >= zeroes.Length then
            '1'
        else
            '0'

    let invert string =
        string
        |> Seq.map (fun c -> if c = '1' then '0' else '1')
        |> System.String.Concat

    let part1 (input: string) : string =
        let inputLines = lines input

        let transposedInput =
            transpose
            <| List.ofArray (Array.map (List.ofSeq) inputLines)

        let gammaRateString =
            transposedInput
            |> List.map mostCommonBit
            |> System.String.Concat

        let gammaRate =
            System.Convert.ToInt32(gammaRateString, 2)

        let epsilonRate =
            System.Convert.ToInt32(invert gammaRateString, 2)

        gammaRate * epsilonRate |> string

    let rec iterate (input: string list) (eqFun: char -> char -> bool) index =
        let mostCommonDigits =
            input
            |> List.map List.ofSeq
            |> transpose
            |> List.map mostCommonBit

        match input with
        | [ elem ] -> elem
        | elems ->
            iterate (List.filter (fun (s: string) -> eqFun s.[index] mostCommonDigits.[index]) elems) eqFun (index + 1)

    let part2 (input: string) : string =
        let inputLines = lines input

        let oxygenRatingString = iterate (List.ofArray inputLines) (=) 0

        let CO2RatingString = iterate (List.ofArray inputLines) (<>) 0

        let CO2Rating =
            System.Convert.ToInt32(CO2RatingString, 2)

        let oxygenRating =
            System.Convert.ToInt32(oxygenRatingString, 2)

        CO2Rating * oxygenRating |> string
