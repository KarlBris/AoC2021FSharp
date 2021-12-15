namespace AoC2021

open Utils

module Day8 =

    let makeInputLine (input: string) : (string [] * string []) =
        let (defs, digs) = input.Split(" | ") |> twoArrayToTuple
        (defs |> words, digs |> words)

    let countEasyDigits (input: (string [] * string [])) : int =
        input
        |> snd
        |> Array.filter (fun s -> Array.contains s.Length [| 2; 3; 4; 7 |])
        |> Array.length

    let part1 (input: string) : string =
        input
        |> lines
        |> Array.map makeInputLine
        |> Array.map countEasyDigits
        |> Array.sum
        |> string

    let segmentsInCommon (def1: string) (def2: string) : int =
        def1
        |> Seq.filter (fun t -> def2 |> Seq.exists (fun t2 -> t = t2))
        |> Seq.length

    let getTranslations (defs: string []) : (string * string) [] =
        let one =
            defs |> Array.find (fun s -> s.Length = 2)

        let four =
            defs |> Array.find (fun s -> s.Length = 4)

        let seven =
            defs |> Array.find (fun s -> s.Length = 3)

        let eight =
            defs |> Array.find (fun s -> s.Length = 7)

        let two =
            defs
            |> Array.find
                (fun s ->
                    segmentsInCommon s one = 1
                    && segmentsInCommon s four = 2
                    && segmentsInCommon s seven = 2
                    && s.Length = 5)

        let three =
            defs
            |> Array.find
                (fun s ->
                    segmentsInCommon s one = 2
                    && segmentsInCommon s four = 3
                    && segmentsInCommon s seven = 3
                    && s.Length = 5)

        let five =
            defs
            |> Array.find
                (fun s ->
                    segmentsInCommon s one = 1
                    && segmentsInCommon s four = 3
                    && segmentsInCommon s seven = 2
                    && s.Length = 5)

        let six =
            defs
            |> Array.find (fun s -> segmentsInCommon s one = 1 && s.Length = 6)

        let zero =
            defs
            |> Array.find
                (fun s ->
                    segmentsInCommon s one = 2
                    && segmentsInCommon s four = 3
                    && segmentsInCommon s seven = 3
                    && s.Length = 6)

        let nine =
            defs
            |> Array.find (fun s -> s.Length = 6 && s <> six && s <> zero)

        [| (one, "1")
           (two, "2")
           (three, "3")
           (four, "4")
           (five, "5")
           (six, "6")
           (seven, "7")
           (eight, "8")
           (nine, "9")
           (zero, "0") |]

    let translate (digs: string []) (translations: (string * string) []) : string [] =
        digs
        |> Array.map
            (fun dig ->
                Array.find
                    (fun (disp, _) -> (disp |> Array.ofSeq |> Array.sort) = (dig |> Array.ofSeq |> Array.sort))
                    translations
                |> snd)

    let getDigitCount (line: (string [] * string [])) : int =
        let (defs, digs) = line

        defs
        |> getTranslations
        |> translate digs
        |> Array.fold (fun s ns -> $"{s}{ns}") ""
        |> int

    let part2 (input: string) : string =
        input
        |> lines
        |> Array.map makeInputLine
        |> Array.map getDigitCount
        |> Array.sum
        |> string
