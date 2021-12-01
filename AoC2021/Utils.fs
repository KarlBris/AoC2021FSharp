namespace AoC2021

open System

module Utils =

    let lines (input: string) : string [] =
        input.Split([| "\r\n"; "\n"; "\r" |], StringSplitOptions.RemoveEmptyEntries)
