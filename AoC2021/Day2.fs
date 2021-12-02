namespace AoC2021

open Utils

module Day2 =
    type Instruction =
        | U of int
        | D of int
        | F of int

    let parseInput (input: string) : Instruction =
        let [| operand; amount |] = input.Split " "

        match operand with
        | "forward" -> F(int amount)
        | "down" -> D(int amount)
        | "up" -> U(int amount)

    let followInstruction (state: int * int) (instruction: Instruction) : (int * int) =
        let (x, y) = state

        match instruction with
        | F n -> (x + n, y)
        | D n -> (x, y + n)
        | U n -> (x, y - n)

    let followInstructionAim (state: int * int * int) (instruction: Instruction) : (int * int * int) =
        let (x, y, a) = state

        match instruction with
        | F n -> (x + n, y + (a * n), a)
        | D n -> (x, y, a + n)
        | U n -> (x, y, a - n)

    let part1 (input: string) : string =
        input
        |> lines
        |> Array.map parseInput
        |> Array.fold followInstruction (0, 0)
        |> (fun (x, y) -> x * y)
        |> string

    let part2 (input: string) : string =
        input
        |> lines
        |> Array.map parseInput
        |> Array.fold followInstructionAim (0, 0, 0)
        |> (fun (x, y, _) -> x * y)
        |> string
