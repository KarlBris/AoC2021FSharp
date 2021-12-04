namespace AoC2021

open Utils
open FSharp.Collections

module Day4 =

    type BingoNumber =
        | Marked of int
        | Normal of int

    let makeBingoBoard (input: string) =
        input
        |> lines
        |> Array.map words
        |> Array.map (Array.map (fun s -> Normal(int s)))
        |> array2D

    let processInput (input: string) =
        let splitInput = input.Split("\n\n")

        ((Array.head splitInput).Split(",")
         |> Array.map int,
         Array.tail splitInput |> Array.map makeBingoBoard)

    let applyNumber (num: int) (board: BingoNumber [,]) : BingoNumber [,] =
        board
        |> Array2D.map
            (fun bingoNum ->
                match bingoNum with
                | Normal a -> if a = num then Marked a else Normal a
                | a -> a)

    let allMarked (numbers: BingoNumber []) =
        numbers
        |> Array.forall
            (fun bingoNum ->
                match bingoNum with
                | Marked _ -> true
                | _ -> false)

    let isWinner (board: BingoNumber [,]) : bool =
        let rowsAndCols =
            [| board.[0, *]
               board.[1, *]
               board.[2, *]
               board.[3, *]
               board.[4, *]
               board.[*, 0]
               board.[*, 1]
               board.[*, 2]
               board.[*, 3]
               board.[*, 4] |]

        (rowsAndCols
         |> Array.filter allMarked
         |> Array.length) > 0

    let calculateAnswer number board =
        (board
         |> Array2D.map
             (fun bingoNum ->
                 match bingoNum with
                 | Normal a -> a
                 | _ -> 0)
         |> Seq.cast<int>
         |> Seq.sum)
        * number

    let rec playBingoToWin (state: int [] * BingoNumber [,] []) =
        let (nums, boards) = state

        let calledNumber = Array.head nums

        let newBoards =
            boards |> Array.map (applyNumber calledNumber)

        let maybeWinnerBoard = newBoards |> Array.tryFind isWinner

        match maybeWinnerBoard with
        | Some board -> calculateAnswer calledNumber board
        | _ -> playBingoToWin (Array.tail nums, newBoards)

    let part1 (input: string) : string =
        input |> processInput |> playBingoToWin |> string

    let rec playBingoToLose (winnerScores: int list) (state: int [] * BingoNumber [,] []) =
        let (nums, boards) = state

        match nums with
        | [||] -> List.last winnerScores
        | _ ->
            let calledNumber = Array.head nums

            let newBoards =
                boards |> Array.map (applyNumber calledNumber)

            let (winnerBoards, restBoards) = newBoards |> Array.partition isWinner

            playBingoToLose
                (winnerScores
                 @ (winnerBoards
                    |> Array.map (calculateAnswer calledNumber)
                    |> List.ofArray))
                (Array.tail nums, restBoards)

    let part2 (input: string) : string =
        input
        |> processInput
        |> playBingoToLose []
        |> string
