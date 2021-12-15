namespace AoC2021

open Utils

module Day12 =

    type Graph = (string * string) list

    let makeGraph (input: string) : Graph =
        input
        |> lines
        |> List.ofArray
        |> List.map (fun s -> s.Split("-"))
        |> List.map twoArrayToTuple

    let canVisitAgain (canVisitTwice: string) (cave: string) (path: string list) : bool =
        if isAllUppercase cave then
            true
        else
            path |> List.filter ((=) cave) |> List.length < (if cave = canVisitTwice then 2 else 1)

    let identifyPaths (canVisitTwice: string) (graph: Graph) (path: string list) : string list =
        let location = List.head path

        if location = "end" then
            []
        else
            graph
            |> List.choose
                (fun (a, b) ->
                    if a = location then Some b
                    elif b = location then Some a
                    else None)
            |> List.filter
                (fun cave ->
                    canVisitAgain canVisitTwice cave path
                    && cave <> "start")


    let rec delve (graph: Graph) (path: string list) (canVisitTwice: string) : (string list) [] =
        let nextSteps =
            path |> identifyPaths canVisitTwice graph

        match nextSteps with
        | [] -> [| path |]
        | steps ->
            steps
            |> Array.ofList
            |> Array.map (fun step -> step :: path)
            |> Array.map (fun x -> delve graph x canVisitTwice)
            |> Array.concat

    let part1 (input: string) : string =
        delve (input |> makeGraph) [ "start" ] ""
        |> Array.filter (fun l -> List.head l = "end")
        |> Array.length
        |> string

    let smallCaves (input: string) : string [] =
        input
        |> lines
        |> Array.map (fun s -> s.Split("-"))
        |> Array.concat
        |> Array.distinct
        |> Array.filter
            (fun s ->
                not (isAllUppercase s)
                && s <> "end"
                && s <> "start")

    let part2 (input: string) : string =
        smallCaves input
        |> Array.map (delve (input |> makeGraph) [ "start" ])
        |> Array.map (Array.filter (fun l -> List.head l = "end"))
        |> Array.concat
        |> Array.distinct
        |> Array.length
        |> string
