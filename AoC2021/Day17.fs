namespace AoC2021

open Utils

module Day17 =

    let xVelLowerBound (lowerX: int) : int =
        int (
            ceil ((sqrt ((8.0 * (float lowerX)) + 1.0)) - 1.0)
            / 2.0
        )

    let rec evalYVel (lowerY: int) (upperY: int) (yPos: int) (highest: int) (yVel: int) : Option<int> =
        if yPos > upperY then
            let yPos2 = yPos + yVel
            let highest2 = max yPos2 highest

            evalYVel lowerY upperY yPos2 highest2 (yVel - 1)
        else if yPos >= lowerY then
            Some highest
        else
            None

    let rec guessYVelocities (lowerY: int) (upperY: int) (yVel: int) : int * int =
        [| 0 .. 150 |]
        |> Array.map (fun yVel -> yVel, evalYVel lowerY upperY 0 0 yVel)
        |> Array.filter (fun (_, op) -> Option.isSome op)
        |> Array.map (fun (v, op) -> (v, Option.get op))
        |> Array.maxBy snd

    let processInput (input: string) =
        input.[13..].Split(", ")
        |> Seq.map
            (fun s ->
                s.[2..].Split("..")
                |> Array.ofSeq
                |> Array.map int
                |> twoArrayToTuple)
        |> Array.ofSeq
        |> twoArrayToTuple

    let part1 (input: string) : string =
        let (_, (ymin, ymax)) = processInput input

        let (_, highest) = guessYVelocities ymin ymax 0

        highest |> string

    let stepXVel vel =
        if vel = 0 then 0
        else if vel < 0 then vel + 1
        else vel - 1

    let rec eval xMin xMax yMin yMax xPos yPos vel =
        let (xVel, yVel) = vel

        if yPos >= yMin && xPos <= xMax then
            if yPos <= yMax && xPos >= xMin then
                true
            else
                eval xMin xMax yMin yMax (xPos + xVel) (yPos + yVel) (stepXVel xVel, yVel - 1)
        else
            false

    let part2 (input: string) : string =
        let ((xmin, xmax), (ymin, ymax)) = processInput input

        let lowestX = xVelLowerBound xmin

        let (highestY, _) = guessYVelocities ymin ymax 0

        Array.allPairs [| lowestX .. xmax |] [| ymin .. highestY |]
        |> Array.map (eval xmin xmax ymin ymax 0 0)
        |> Array.filter id
        |> Array.length
        |> string
