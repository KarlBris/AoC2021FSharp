namespace AoC2021

open System.IO
open System.Diagnostics

module TestRunner =

    let getInput day =
        let filename = $"..\..\..\inputs\input_day{day}.txt"

        if File.Exists filename then
            filename
            |> File.ReadAllText
            |> String.filter (fun c -> c <> '\r')
        else
            failwith $"Input file {filename} not found"

    let makeComparison (expectedResults: string []) (results: string []) =
        Array.zip results expectedResults
        |> Array.map (fun (r, e) -> (r, e, r = e))

    let printStatus ((res, expectedRes, success): string * string * bool) =
        printfn "%s! Got %s, expected %s." (if success then "Success" else "Failure") res expectedRes

    let run (examples: string []) expectedResults realInput (func: string -> string) title =
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


    // Day 8
    let input8 = getInput 8

    let examples8_1 =
        [| "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe\nedbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc\nfgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg\nfbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb\naecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea\nfgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb\ndbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe\nbdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef\negadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb\ngcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce" |]

    let exampleResults8_1 = [| "26" |]

    let examples8_2 = examples8_1

    let exampleResults8_2 = [| "61229" |]


    // Day 9
    let input9 = getInput 9

    let examples9_1 =
        [| "2199943210\n3987894921\n9856789892\n8767896789\n9899965678" |]

    let exampleResults9_1 = [| "15" |]

    let examples9_2 = examples9_1

    let exampleResults9_2 = [| "1134" |]


    // Day 10
    let input10 = getInput 10

    let examples10_1 =
        [| "[({(<(())[]>[[{[]{<()<>>\n[(()[<>])]({[<{<<[]>>(\n{([(<{}[<>[]}>{[]{[(<()>\n(((({<>}<{<{<>}{[]{[]{}\n[[<[([]))<([[{}[[()]]]\n[{[{({}]{}}([{[{{{}}([]\n{<[[]]>}<{[{[{[]{()[[[]\n[<(<(<(<{}))><([]([]()\n<{([([[(<>()){}]>(<<{{\n<{([{{}}[<[[[<>{}]]]>[]]" |]

    let exampleResults10_1 = [| "26397" |]

    let examples10_2 = examples10_1

    let exampleResults10_2 = [| "288957" |]


    // Day 11
    let input11 = getInput 11

    let examples11_1 =
        [| "5483143223\n2745854711\n5264556173\n6141336146\n6357385478\n4167524645\n2176841721\n6882881134\n4846848554\n5283751526" |]

    let exampleResults11_1 = [| "1656" |]

    let examples11_2 = examples11_1

    let exampleResults11_2 = [| "195" |]


    // Day 12
    let input12 = getInput 12

    let examples12_1 =
        [| "start-A\nstart-b\nA-c\nA-b\nb-d\nA-end\nb-end"
           "dc-end\nHN-start\nstart-kj\ndc-start\ndc-HN\nLN-dc\nHN-end\nkj-sa\nkj-HN\nkj-dc"
           "fs-end\nhe-DX\nfs-he\nstart-DX\npj-DX\nend-zg\nzg-sl\nzg-pj\npj-he\nRW-he\nfs-DX\npj-RW\nzg-RW\nstart-pj\nhe-WI\nzg-he\npj-fs\nstart-RW" |]

    let exampleResults12_1 = [| "10"; "19"; "226" |]

    let examples12_2 = examples12_1

    let exampleResults12_2 = [| "36"; "103"; "3509" |]


    // Day 13
    let input13 = getInput 13

    let examples13_1 =
        [| "6,10\n0,14\n9,10\n0,3\n10,4\n4,11\n6,0\n6,12\n4,1\n0,13\n10,12\n3,4\n3,0\n8,4\n1,10\n2,14\n8,10\n9,0\n\nfold along y=7\nfold along x=5" |]

    let exampleResults13_1 = [| "17" |]

    let examples13_2 = examples13_1

    let exampleResults13_2 = [| "" |]


    // Day 14
    let input14 = getInput 14

    let examples14_1 =
        [| "NNCB\n\nCH -> B\nHH -> N\nCB -> H\nNH -> C\nHB -> C\nHC -> B\nHN -> C\nNN -> C\nBH -> H\nNC -> B\nNB -> B\nBN -> B\nBB -> N\nBC -> B\nCC -> N\nCN -> C" |]

    let exampleResults14_1 = [| "1588" |]

    let examples14_2 = examples14_1

    let exampleResults14_2 = [| "2188189693529" |]


    // Day 15
    let input15 = getInput 15

    let examples15_1 =
        [| "1163751742\n1381373672\n2136511328\n3694931569\n7463417111\n1319128137\n1359912421\n3125421639\n1293138521\n2311944581" |]

    let exampleResults15_1 = [| "40" |]

    let examples15_2 = examples15_1

    let exampleResults15_2 = [| "315" |]


    // Day 16
    let input16 = getInput 16

    let examples16_1 =
        [| "A0016C880162017C3686B18A3D4780"
           "8A004A801A8002F478"
           "620080001611562C8802118E34"
           "C0015000016115A2E0802F182340"
           "A0016C880162017C3686B18A3D4780" |]

    let exampleResults16_1 = [| "31"; "16"; "12"; "23"; "31" |]

    let examples16_2 =
        [| "C200B40A82"
           "04005AC33890"
           "880086C3E88112"
           "CE00C43D881120"
           "D8005AC2A8F0"
           "F600BC2D8F"
           "9C005AC2F8F0"
           "9C0141080250320F1802104A08" |]

    let exampleResults16_2 =
        [| "3"
           "54"
           "7"
           "9"
           "1"
           "0"
           "0"
           "1" |]
