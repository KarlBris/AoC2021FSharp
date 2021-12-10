namespace AoC2021

open Utils

module Day10 =

    type LineResult =
        | Incomplete of char list
        | Error of char

    let illegalScore (c: char) : int =
        match c with
        | ')' -> 3
        | ']' -> 57
        | '}' -> 1197
        | '>' -> 25137
        | _ -> 0

    let reverseParen (c: char) : char =
        match c with
        | '(' -> ')'
        | '[' -> ']'
        | '{' -> '}'
        | '<' -> '>'
        | _ -> failwith "reverseParen error"

    let isLeftParen (c: char) : bool =
        match c with
        | '(' -> true
        | '[' -> true
        | '{' -> true
        | '<' -> true
        | _ -> false

    let matchWithQueue (queue: char list) (paren: char) =
        let (qh :: qt) = queue

        if isLeftParen paren then
            Some(paren :: queue)
        else if reverseParen qh = paren then
            Some qt
        else
            None

    let rec parseLine (queue: char list) (input: char []) : LineResult =
        match queue with
        | [] -> parseLine ([ Array.head input ]) (Array.tail input)
        | _ ->
            match input with
            | [||] -> Incomplete queue
            | a ->
                match matchWithQueue queue (Array.head a) with
                | Some q -> parseLine q (Array.tail a)
                | None -> Error(Array.head a)

    let isError (v: LineResult) : bool =
        match v with
        | Error _ -> true
        | _ -> false

    let getError (v: LineResult) : char =
        match v with
        | Error c -> c
        | _ -> failwith "getError error"

    let getIncomplete (v: LineResult) : char list =
        match v with
        | Incomplete c -> c
        | _ -> failwith "getIncomplete error"

    let part1 (input: string) : string =
        input
        |> lines
        |> Array.map Array.ofSeq
        |> Array.map (parseLine [])
        |> Array.filter isError
        |> Array.map (getError)
        |> Array.map illegalScore
        |> Array.sum
        |> string

    let getPoints (c: char) : int64 =
        match c with
        | ')' -> 1L
        | ']' -> 2L
        | '}' -> 3L
        | '>' -> 4L
        | _ -> 0L

    let makeScore (scores: char list) : int64 =
        List.fold (fun s c -> (s * 5L) + (getPoints c)) 0L scores

    let getMiddle (scores: int64 []) : int64 = scores.[scores.Length / 2]

    let part2 (input: string) : string =
        input
        |> lines
        |> Array.map Array.ofSeq
        |> Array.map (parseLine [])
        |> Array.filter (fun e -> not (isError e))
        |> Array.map getIncomplete
        |> Array.map (List.map reverseParen)
        |> Array.map makeScore
        |> Array.sort
        |> getMiddle
        |> string
