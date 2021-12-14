namespace AoC2021

open Utils

module Day14 =

    type Rules = Map<(char * char), char>
    type Memo = Map<(char * char) * int, Map<char, int64>>
    type OccurrenceMap = Map<char, int64>

    let makeRule (ruleString: string) : ((char * char) * char) =
        let (fromRule, toRule) =
            ruleString.Split(" -> ") |> twoArrayToTuple

        (fromRule |> Array.ofSeq |> twoArrayToTuple, toRule |> char)

    let addToMap key value map =
        Map.change
            key
            (fun op ->
                match op with
                | Some v -> Some(v + value)
                | None -> Some(value))
            map

    let addMaps (mapA: OccurrenceMap) (mapB: OccurrenceMap) : OccurrenceMap =
        mapA
        |> Map.fold (fun state key v -> addToMap key v state) mapB

    let combineMemos (memo1: OccurrenceMap) (memo2: OccurrenceMap) (removeOneOfThese: char) : OccurrenceMap =
        addMaps memo1 memo2
        |> addToMap removeOneOfThese (-1L)

    let rec polymerize (start: char * char) (rules: Rules) (iters: int) (memo: Memo) : Memo =
        let (left, right) = start
        let a = Map.find start rules

        match Map.tryFind (start, iters) memo with
        | Some v -> memo
        | None ->
            match iters with
            | 1 ->
                let b =
                    [| left; a; right |]
                    |> Array.fold (fun map mol -> addToMap mol 1L map) Map.empty

                (Map.add ((left, right), iters) b memo)
            | _ ->
                let leftPair = (left, a)
                let rightPair = (a, right)

                let memo2 =
                    polymerize leftPair rules (iters - 1) memo
                    |> polymerize rightPair rules (iters - 1)

                let combinedMemo =
                    combineMemos (Map.find (leftPair, iters - 1) memo2) (Map.find (rightPair, iters - 1) memo2) a

                Map.add ((left, right), iters) combinedMemo memo2

    let polymerization start rules runs =

        let pairs = start |> Array.ofSeq |> Array.pairwise

        let (_, result) =
            pairs
            |> Array.fold
                (fun (myMemo, acc) pair ->
                    let (baz, quux) = pair
                    let foo = polymerize pair rules runs myMemo

                    let bar2 =
                        addMaps acc (Map.find (pair, runs) foo)
                        |> addToMap quux (-1L)

                    (foo, bar2))
                (Map.empty, Map.empty)

        addToMap (start |> Array.ofSeq |> Array.last) 1L result
        |> Map.values
        |> Array.ofSeq
        |> Array.sort

    let processInput (input: string) : (string * Rules) =
        let (start, rulesString) = input.Split("\n\n") |> twoArrayToTuple

        let rules =
            rulesString
            |> lines
            |> Array.map makeRule
            |> Map.ofArray

        (start, rules)

    let part1 (input: string) : string =
        let (start, rules) = processInput input

        let b = polymerization start rules 10

        (Array.last b) - (Array.head b) |> string

    let part2 (input: string) : string =
        let (start, rules) = processInput input

        let b = polymerization start rules 40

        (Array.last b) - (Array.head b) |> string
