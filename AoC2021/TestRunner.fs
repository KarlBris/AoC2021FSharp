namespace AoC2021

open System.IO

module TestRunner =

    let getInput day =
        let filename = $"input_day{day}.txt"

        if File.Exists filename then
            File.ReadAllText filename
        else
            failwith $"Input file input_day{day}.txt not found"

    let makeComparison (expectedResults: string []) (results: string []) =
        Array.zip results expectedResults
        |> Array.map (fun (r, e) -> (r, e, r = e))

    let printStatus ((res, expectedRes, success): string * string * bool) =
        printfn "%s! Got %s, expected %s." (if success then "Success" else "Failure") res expectedRes

    let runExamplesAndMain examples expectedResults realInput (func: string -> string) title =
        let resultList =
            examples
            |> Array.map func
            |> makeComparison expectedResults

        printfn title

        if examples.Length = 0 then
            printfn "No examples found, running the real input..."
        else
            printfn "Running and verifying examples before the real input..."

        resultList |> Array.map printStatus |> ignore

        let examplesSuccessful =
            resultList
            |> Array.fold (fun b1 (_, _, b2) -> b1 && b2) true

        if examplesSuccessful then
            printfn "All examples were successful. Result from real input: %s" (func realInput)
        else
            printfn "Some examples were not successful. PLEASE DO BETTER"

    // Day1
    let input1 = getInput 1

    let examples1_1 =
        [| @"199
    200
    208
    210
    200
    207
    240
    269
    260
    263" |]

    let exampleResults1_1 = [| "7" |]

    let examples1_2 =
        [| @"199
    200
    208
    210
    200
    207
    240
    269
    260
    263" |]

    let exampleResults1_2 = [| "5" |]


    // Day 2
    let input2 = "" //getInput 2

    let examples2_1 = [||]

    let exampleResults2_1 = [||]

    let examples2_2 = [||]

    let exampleResults2_2 = [||]
