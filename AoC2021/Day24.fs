namespace AoC2021

open Utils

module Day24 =

    type Source =
        | V of char
        | I of int

    type Instruction =
        | INP of char
        | ADD of (char * Source)
        | MUL of (char * Source)
        | DIV of (char * Source)
        | MOD of (char * Source)
        | EQL of (char * Source)

    type ProgramState = Map<char, int>

    let parseSource (source: string) : Source =
        if source = "w"
           || source = "x"
           || source = "y"
           || source = "z" then
            V source.[0]
        else
            I(int source)

    let parseInstruction (line: string) : Instruction =
        let foo = line.Split(" ")

        match foo.[0] with
        | "inp" -> INP foo.[1].[0]
        | "add" -> ADD(foo.[1].[0], (parseSource foo.[2]))
        | "mul" -> MUL(foo.[1].[0], (parseSource foo.[2]))
        | "div" -> DIV(foo.[1].[0], (parseSource foo.[2]))
        | "mod" -> MOD(foo.[1].[0], (parseSource foo.[2]))
        | "eql" -> EQL(foo.[1].[0], (parseSource foo.[2]))

    let getVal (s: Source) (state: ProgramState) : int =
        match s with
        | V c -> Map.find c state
        | I v -> v

    let rec runCode (input: int list) (state: ProgramState) (instructions: Instruction list) : bool =
        if List.contains 0 input then
            false
        else
            match instructions with
            | [] -> Map.find 'z' state = 0
            | i :: is ->
                match i with
                | INP v -> runCode (List.tail input) (Map.add v (List.head input) state) is
                | ADD (vr, vl) -> runCode input (Map.add vr ((getVal vl state) + (Map.find vr state)) state) is
                | MUL (vr, vl) -> runCode input (Map.add vr ((getVal vl state) * (Map.find vr state)) state) is
                | DIV (vr, vl) -> runCode input (Map.add vr ((Map.find vr state) / (getVal vl state)) state) is
                | MOD (vr, vl) -> runCode input (Map.add vr ((Map.find vr state) % (getVal vl state)) state) is
                | EQL (vr, vl) ->
                    runCode
                        input
                        (Map.add
                            vr
                            (if (getVal vl state) = (Map.find vr state) then
                                 1
                             else
                                 0)
                            state)
                        is

    let rec loop modelNumber instructions =
        let modelInput =
            modelNumber
            |> string
            |> List.ofSeq
            |> List.map (fun c -> int c - 48)

        let res =
            runCode
                modelInput
                (Map.ofList [ ('w', 0)
                              ('x', 0)
                              ('y', 0)
                              ('z', 0) ])
                instructions

        if res then
            modelNumber
        else
            loop (modelNumber - 1L) instructions

    let part1 (input: string) : string =
        let instructions =
            input
            |> lines
            |> Array.map parseInstruction
            |> List.ofArray

        let modelNumber = 99999999999999L

        if instructions.Length > 0 then
            loop modelNumber instructions |> string
        else
            ""

    let part2 (input: string) : string = input
