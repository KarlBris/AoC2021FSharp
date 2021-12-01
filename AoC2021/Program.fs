open System
open AoC2021
open TestRunner

[<EntryPoint>]
let main argv =
    
    //let res = TestRunner.makeComparison TestRunner.exampleResults1_1 (List.map Day1.part1 TestRunner.examples1_1)
    //let res = Day1.part1 TestRunner.input1
    
    runExamplesAndMain TestRunner.examples1_1 TestRunner.exampleResults1_1 TestRunner.input1 Day1.part1 "Day 1 part 1"
    runExamplesAndMain TestRunner.examples1_2 TestRunner.exampleResults1_2 TestRunner.input1 Day1.part2 "Day 1 part 2"

    //printfn "%A" res
    0 // return an integer exit code
