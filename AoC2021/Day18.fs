namespace AoC2021

open Utils

module Day18 =

    type SnailNum =
        | Num of int
        | Pair of SnailNum * SnailNum

    type PassOnFromDir =
        | Left
        | Right

    let rec magnitude (sNum: SnailNum) : int =
        match sNum with
        | Num n -> n
        | Pair (sn1, sn2) -> (3 * (magnitude sn1)) + (2 * (magnitude sn2))

    let rec passOnFrom (fromDir: PassOnFromDir) (passOnValue: int) (sNum: SnailNum) : SnailNum =
        match sNum with
        | Num a -> Num(a + passOnValue)
        | Pair (sn1, sn2) ->
            match fromDir with
            | Right -> Pair(passOnFrom fromDir passOnValue sn1, sn2)
            | Left -> Pair(sn1, passOnFrom fromDir passOnValue sn2)

    // returns (hasExploded*passOnLeft*passOnRight,explosionResult)
    let rec explode (exploded: bool) (depth: int) (sNum: SnailNum) : (bool * int * int * SnailNum) =
        match sNum with
        | Num _ -> (exploded, 0, 0, sNum)
        | Pair (snL, snR) ->
            if depth >= 4 && (not exploded) then
                let nL = (fun (Num s) -> s) snL
                let nR = (fun (Num s) -> s) snR
                (true, nL, nR, Num 0)
            else
                let (explodedL, snLpassOnLeft, snLpassOnRight, snL2) = explode exploded (depth + 1) snL

                if explodedL then // left element exploded
                    (true, snLpassOnLeft, 0, Pair(snL2, passOnFrom Right snLpassOnRight snR))
                else
                    let (explodedR, snRpassOnLeft, snRpassOnRight, snR2) = explode exploded (depth + 1) snR

                    if explodedR then // right element exploded
                        (true, 0, snRpassOnRight, Pair(passOnFrom Left snRpassOnLeft snL, snR2))
                    else
                        (false, 0, 0, Pair(snL2, snR2))

    let rec split (hasSplit: bool) (sNum: SnailNum) : bool * SnailNum =
        match sNum with
        | Pair (sn1, sn2) ->
            let (split1, sn1') = split false sn1

            if split1 then
                (true, Pair(sn1', sn2))
            else
                let (split2, sn2') = split false sn2

                if split2 then
                    (true, Pair(sn1, sn2'))
                else
                    (false, Pair(sn1, sn2))
        | Num n ->
            if n > 9 && (not hasSplit) then
                let div = (float n) / 2.0
                (true, Pair(Num(int (floor div)), Num(int (ceil div))))
            else
                (hasSplit, sNum)

    let rec reduce (sNum: SnailNum) : SnailNum =
        let (exploded, _, _, sNum2) = explode false 0 sNum

        if exploded then
            reduce sNum2
        else
            let (hasSplit, sNum3) = split false sNum2

            if hasSplit then reduce sNum3 else sNum3

    let add (sNum1: SnailNum) (sNum2: SnailNum) : SnailNum = Pair(sNum1, sNum2) |> reduce

    let rec findNonbracketComma (brackets: int) (index: int) (line: char list) : int =
        match line with
        | ',' :: rest ->
            match brackets with
            | 0 -> index
            | _ -> findNonbracketComma brackets (index + 1) rest // go look for more brackets
        | '[' :: rest -> findNonbracketComma (brackets + 1) (index + 1) rest
        | ']' :: rest -> findNonbracketComma (brackets - 1) (index + 1) rest
        | c :: rest -> findNonbracketComma brackets (index + 1) rest
        | [] -> failwith "empty string"

    let rec makeSnailNum (line: string) : SnailNum =
        let commas =
            line
            |> Seq.filter (fun c -> c = ',')
            |> Seq.length

        if commas = 0 then

            line |> int |> Num

        else
            let bracketlessLine = line.[1..(line.Length - 2)]

            let index =
                bracketlessLine
                |> List.ofSeq
                |> findNonbracketComma 0 0

            let left = bracketlessLine.[0..(index - 1)]

            let right =
                bracketlessLine.[(index + 1)..(bracketlessLine.Length - 1)]

            Pair((makeSnailNum left), (makeSnailNum right))

    let part1 (input: string) : string =
        let nums = input |> lines |> Array.map makeSnailNum

        let reducedSum =
            Array.tail nums
            |> Array.fold (fun acc sNum -> add acc sNum) (Array.head nums)

        reducedSum |> magnitude |> string

    let part2 (input: string) : string =
        let nums = input |> lines |> Array.map makeSnailNum
        let pairs = Array.allPairs nums nums

        pairs
        |> Array.map (fun (n1, n2) -> add n1 n2 |> magnitude)
        |> Array.sortDescending
        |> Array.head
        |> string
