namespace AoC2021

open Utils

module Day24 =

    type Source =
        | V of char
        | I of int64

    type Instruction =
        | INP of char
        | ADD of (char * Source)
        | MUL of (char * Source)
        | DIV of (char * Source)
        | MOD of (char * Source)
        | EQL of (char * Source)

    type ProgramState = Map<char, int64>

    let parseSource (source: string) : Source =
        if source = "w"
           || source = "x"
           || source = "y"
           || source = "z" then
            V source.[0]
        else
            I(int64 source)

    let parseInstruction (line: string) : Instruction =
        let foo = line.Split(" ")

        match foo.[0] with
        | "inp" -> INP foo.[1].[0]
        | "add" -> ADD(foo.[1].[0], (parseSource foo.[2]))
        | "mul" -> MUL(foo.[1].[0], (parseSource foo.[2]))
        | "div" -> DIV(foo.[1].[0], (parseSource foo.[2]))
        | "mod" -> MOD(foo.[1].[0], (parseSource foo.[2]))
        | "eql" -> EQL(foo.[1].[0], (parseSource foo.[2]))

    let getVal (s: Source) (state: ProgramState) : int64 =
        match s with
        | V c -> Map.find c state
        | I v -> v

    let rec runCode (input: int64 list) (state: ProgramState) (instructions: Instruction list) : int64 =
        match instructions with
        | [] -> Map.find 'z' state
        | i :: is ->
            match i with
            | INP v -> runCode (List.tail input) (Map.add v (List.head input) state) is
            | ADD (vr, vl) -> runCode input (Map.add vr ((getVal vl state) + (Map.find vr state)) state) is
            | MUL (vr, vl) -> runCode input (Map.add vr ((getVal vl state) * (Map.find vr state)) state) is
            | DIV (vr, vl) -> runCode input (Map.add vr ((Map.find vr state) / (getVal vl state)) state) is
            | MOD (vr, vl) -> runCode input (Map.add vr (eMod64 (Map.find vr state) (getVal vl state)) state) is
            | EQL (vr, vl) ->
                runCode
                    input
                    (Map.add
                        vr
                        (if (getVal vl state) = (Map.find vr state) then
                             1L
                         else
                             0L)
                        state)
                    is

    let calcStep ((a, b, c): (int64 * int64 * int64)) (z0: int64) (input: int64) : int64 =

        let x =
            if ((eMod64 z0 26L) + b) = input then
                0L
            else
                1L

        if x = 0L then
            printfn "x is 0 for input %d. (b = %d)" input b

        (z0 / a) * (((25L * x) + 1L)) + ((input + c) * x)

    let rec loop lowest modelNumber triples instructions =
        let _asd =
            if modelNumber % 1000L = 0L then
                printfn "model number = %d. Lowest = %d" modelNumber lowest
            else
                ()

        let modelInputPre =
            modelNumber
            |> string
            |> List.ofSeq
            |> List.map (fun c -> int64 c - 48L)

        let modelInput =
            modelInputPre.[0..3]
            @ [ 1; 2 ]
              @ [ modelInputPre.[4] ]
                @ [ 3; 4; 5 ]
                  @ [ modelInputPre.[5] ]
                    @ [ 6 ] @ [ (*modelInputPre.[6]*) 8 ] @ [ 7 ]

        if List.contains 0L modelInput then
            loop lowest (modelNumber - 1L) triples instructions
        else
            let inputs = List.zip triples modelInput

            let res =
                List.fold (fun s (trips, inp) -> calcStep trips s inp) 0L inputs

            (*let codeRes =
                runCode
                    modelInput
                    (Map.ofList [ ('w', 0L)
                                  ('x', 0L)
                                  ('y', 0L)
                                  ('z', 0L) ])
                    instructions
            *)

            if res = 0L then
                modelInput
            else
                loop (min lowest res) (modelNumber - 1L) triples instructions

    let part1 (input: string) : string =
        let aas =
            [ 1L
              1L
              1L
              1L
              26L
              26L
              1L
              26L
              26L
              26L
              1L
              26L
              1L
              26L ]

        let abs =
            [ 11L
              13L
              11L
              10L
              -3L
              -4L
              12L
              -8L
              -3L
              -12L
              14L
              -6L
              11L
              -12L ]

        let acs =
            [ 14L
              8L
              4L
              10L
              14L
              10L
              4L
              14L
              1L
              6L
              0L
              9L
              13L
              12L ]

        let triples = List.zip3 aas abs acs

        let modelNumber = 11119919551919L

        let inputs =
            List.zip
                triples
                (modelNumber
                 |> string
                 |> List.ofSeq
                 |> List.map (fun c -> (int64 c) - 48L))

        let foo = calcStep triples.[0] 0L 1L

        let res =
            List.fold (fun s (trips, inp) -> calcStep trips s inp) 0 inputs

        let instructions =
            input
            |> lines
            |> Array.map parseInstruction
            |> List.ofArray


        let modelNumber = 9999999L

        if instructions.Length > 0 then
            loop System.Int64.MaxValue modelNumber triples instructions
            |> string
        else
            ""

    let part2 (input: string) : string = input
