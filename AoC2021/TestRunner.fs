﻿namespace AoC2021

open System.IO
open System.Diagnostics

module TestRunner =

    let getInput day =
        let filename = $"..\..\..\inputs\input_day{day}.txt"

        if File.Exists filename then
            File.ReadAllText filename
        else
            failwith $"Input file {filename} not found"

    let makeComparison (expectedResults: string []) (results: string []) =
        Array.zip results expectedResults
        |> Array.map (fun (r, e) -> (r, e, r = e))

    let printStatus ((res, expectedRes, success): string * string * bool) =
        printfn "%s! Got %s, expected %s." (if success then "Success" else "Failure") res expectedRes

    let runExamplesAndMain (examples: string []) expectedResults realInput (func: string -> string) title =
        printfn title

        if examples.Length = 0 then
            printfn "No examples found, running the real input..."
        else
            printfn "Running and verifying examples before the real input..."

        let resultList =
            examples
            |> Array.map func
            |> makeComparison expectedResults

        resultList |> Array.map printStatus |> ignore

        let examplesSuccessful =
            resultList
            |> Array.fold (fun b1 (_, _, b2) -> b1 && b2) true



        if examplesSuccessful then
            printfn "All examples were successful, running the real input..."
            let timer = new Stopwatch()
            timer.Start()
            printfn "Result from real input: %s" (func realInput)
            timer.Stop()
            printfn "Time elapsed: %A" timer.Elapsed
        else
            printfn "Some examples were not successful. PLEASE DO BETTER"


        printfn ""

    // Day1
    let input1 = getInput 1

    let examples1_1 =
        [| "199\n200\n208\n210\n200\n207\n240\n269\n260\n263" |]

    let exampleResults1_1 = [| "7" |]

    let examples1_2 = examples1_1

    let exampleResults1_2 = [| "5" |]


    // Day 2
    let input2 = getInput 2

    let examples2_1 =
        [| "forward 5\ndown 5\nforward 8\nup 3\ndown 8\nforward 2" |]

    let exampleResults2_1 = [| "150" |]

    let examples2_2 = examples2_1

    let exampleResults2_2 = [| "900" |]


    // Day 3
    let input3 = getInput 3

    let examples3_1 =
        [| "00100\n11110\n10110\n10111\n10101\n01111\n00111\n11100\n10000\n11001\n00010\n01010" |]

    let exampleResults3_1 = [| "198" |]

    let examples3_2 = examples3_1

    let exampleResults3_2 = [| "230" |]


    // Day 4
    let input4 = getInput 4

    let examples4_1 =
        [| "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1\n\n22 13 17 11  0\n 8  2 23  4 24\n21  9 14 16  7\n 6 10  3 18  5\n 1 12 20 15 19\n\n 3 15  0  2 22\n 9 18 13 17  5\n19  8  7 25 23\n20 11 10 24  4\n14 21 16 12  6\n\n14 21 17 24  4\n10 16 15  9 19\n18  8 23 26 20\n22 11 13  6  5\n 2  0 12  3  7" |]

    let exampleResults4_1 = [| "4512" |]

    let examples4_2 = examples4_1

    let exampleResults4_2 = [| "1924" |]


    // Day 5
    let input5 = getInput 5

    let examples5_1 =
        [| "0,9 -> 5,9\n8,0 -> 0,8\n9,4 -> 3,4\n2,2 -> 2,1\n7,0 -> 7,4\n6,4 -> 2,0\n0,9 -> 2,9\n3,4 -> 1,4\n0,0 -> 8,8\n5,5 -> 8,2" |]

    let exampleResults5_1 = [| "5" |]

    let examples5_2 = examples5_1

    let exampleResults5_2 = [| "12" |]


    // Day 6
    let input6 = getInput 6

    let examples6_1 = [| "3,4,3,1,2" |]

    let exampleResults6_1 = [| "5934" |]

    let examples6_2 = examples6_1

    let exampleResults6_2 = [| "26984457539" |]


    // Day 7
    let input7 = getInput 7

    let examples7_1 = [| "16,1,2,0,4,2,7,1,2,14" |]

    let exampleResults7_1 = [| "37" |]

    let examples7_2 = examples7_1

    let exampleResults7_2 = [| "168" |]
