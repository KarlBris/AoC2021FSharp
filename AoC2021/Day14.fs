namespace AoC2021

open Utils

module Day14 =

    type Rules = Map<(char * char), char>
    let runs = 4

    let makeRule (ruleString: string) : ((char * char) * char) =
        let (fromRule, toRule) =
            ruleString.Split(" -> ") |> twoArrayToTuple

        (fromRule |> Array.ofSeq |> twoArrayToTuple, toRule |> char)

    let rec polymerize (start: char []) (rules: Rules) (iters: int) : char [] =
        match iters with
        | 0 -> start
        | _ ->
            let hej =
                start
                |> Array.pairwise
                |> Array.map (fun p -> Map.find p rules)
                |> Array.append [| 'Ö' |]

            let newArray =
                Array.foldBack2 (fun a b xs -> a :: b :: xs) hej start []
                |> List.tail
                |> Array.ofList

            polymerize newArray rules (iters - 1)

    let part1 (input: string) : string =
        let (start, rulesString) = input.Split("\n\n") |> twoArrayToTuple

        let rules =
            rulesString
            |> lines
            |> Array.map makeRule
            |> Map.ofArray

        let startArray = start |> Array.ofSeq

        let result = polymerize startArray rules runs

        let bar =
            result
            |> Array.fold
                (fun map molecule ->
                    Map.change
                        molecule
                        (fun op ->
                            match op with
                            | Some v -> Some(v + 1)
                            | None -> Some(1))
                        map)
                Map.empty
            |> Map.values
            |> Array.ofSeq
            |> Array.sort

        (Array.last bar) - (Array.head bar) |> string

    let rec polymerize2 (start: char * char) (rules: Rules) (iters: int) (acc: Map<char, int64>) : Map<char, int64> =
        let (left, right) = start

        match iters with
        | 1 ->
            let a = Map.find start rules

            let b =
                [| left; a |]
                |> Array.fold
                    (fun map mol ->
                        Map.change
                            mol
                            (fun op ->
                                match op with
                                | Some v -> Some(v + 1L)
                                | None -> Some(1L))
                            map)
                    acc

            b
        | _ ->

            let a = Map.find start rules

            let leftMap =
                polymerize2 (left, a) rules (iters - 1) acc

            let rightMap =
                polymerize2 (a, right) rules (iters - 1) leftMap

            rightMap

    let addMaps (mapA: Map<char, int64>) (mapB: Map<char, int64>) : Map<char, int64> =
        mapA
        |> Map.fold
            (fun state key v ->
                Map.change
                    key
                    (fun op ->
                        match op with
                        | Some value -> Some(value + v)
                        | None -> Some v)
                    state)
            mapB

    let rec polymerize3
        (start: char * char)
        (rules: Rules)
        (iters: int)
        (acc: Map<char, int64>)
        (memo: Map<(char * char) * int, Map<char, int64>>)
        : (Map<char, int64> * (Map<(char * char) * int, Map<char, int64>>)) =
        let (left, right) = start

        match iters with
        | 1 ->
            let a = Map.find start rules

            let b =
                [| left; a; right |]
                |> Array.fold
                    (fun map mol ->
                        Map.change
                            mol
                            (fun op ->
                                match op with
                                | Some v -> Some(v + 1L)
                                | None -> Some(1L))
                            map)
                    acc

            (b, memo)
        | _ ->

            let a = Map.find start rules

            match Map.tryFind (start, iters) memo with
            | Some v -> (addMaps v acc, memo)
            | None ->
                let (leftMap, memo2) =
                    polymerize3 (left, a) rules (iters - 1) acc memo

                let memo3 = Map.add ((left, a), iters) leftMap memo2

                let (rightMap, memo4) =
                    polymerize3 (a, right) rules (iters - 1) acc memo3

                let memo5 =
                    Map.add ((a, right), iters) rightMap memo4


                (rightMap, memo5)

    let part2 (input: string) : string =
        let (start, rulesString) = input.Split("\n\n") |> twoArrayToTuple

        let rules =
            rulesString
            |> lines
            |> Array.map makeRule
            |> Map.ofArray

        let pairs = start |> Array.ofSeq |> Array.pairwise

        let resultP =
            pairs
            |> Array.fold (fun (myMap, myMemo) pair -> polymerize3 pair rules runs myMap myMemo) (Map.empty, Map.empty)

        let result =
            resultP
            |> fst
            |> Map.change
                (Seq.last start)
                (fun opt ->
                    match opt with
                    | Some v -> Some(v + 1L)
                    | None -> Some 1L)

        let bar =
            result |> Map.values |> Array.ofSeq |> Array.sort

        (Array.last bar) - (Array.head bar) |> string
