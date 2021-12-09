namespace AoC2021

open Utils

module Day9 =

    type TraveseInt =
        | Unmarked of (int)
        | Marked

    let isValueLowerThanAdjacents x y v xLen yLen (array: int [,]) : bool =
        let adjacents =
            [| if x > 0 then
                   Array2D.get array (x - 1) y
               else
                   11

               if x < (xLen - 1) then
                   Array2D.get array (x + 1) y
               else
                   11

               if y > 0 then
                   Array2D.get array x (y - 1)
               else
                   11

               if y < (yLen - 1) then
                   Array2D.get array x (y + 1)
               else
                   11 |]

        Array.forall (fun a -> a > v) adjacents

    let part1 (input: string) : string =
        let array =
            input
            |> lines
            |> Array.map (Array.ofSeq)
            |> Array.map (Array.map string)
            |> Array.map (Array.map int)
            |> array2D

        array
        |> Array2D.mapi
            (fun x y v ->
                if isValueLowerThanAdjacents x y v (Array2D.length1 array) (Array2D.length2 array) array then
                    v + 1
                else
                    0)
        |> Seq.cast<int>
        |> Seq.sum
        |> string

    let isOutOfBounds x y array : bool =
        x < 0
        || y < 0
        || x >= (Array2D.length1 array)
        || y >= (Array2D.length2 array)

    let rec floodFill (pos: int * int) (array: TraveseInt [,]) : TraveseInt [,] =
        let (x, y) = pos

        if isOutOfBounds x y array then
            array
        else
            let currentVal = array.[x, y]

            if currentVal = Unmarked 9 || currentVal = Marked then
                array
            else
                array.[x, y] <- Marked

                array
                |> floodFill (x + 1, y)
                |> floodFill (x - 1, y)
                |> floodFill (x, y + 1)
                |> floodFill (x, y - 1)

    let expandBasin (array: int [,]) (pos: int * int) : int =
        let traverseArray = Array2D.map (fun v -> Unmarked v) array

        floodFill pos traverseArray
        |> Seq.cast<TraveseInt>
        |> Seq.filter
            (fun ti ->
                match ti with
                | Marked -> true
                | Unmarked _ -> false)
        |> Seq.length

    let part2 (input: string) : string =
        let array =
            input
            |> lines
            |> Array.map (Array.ofSeq)
            |> Array.map (Array.map string)
            |> Array.map (Array.map int)
            |> array2D

        array
        |> Array2D.mapi
            (fun x y v ->
                if isValueLowerThanAdjacents x y v (Array2D.length1 array) (Array2D.length2 array) array then
                    Some(x, y)
                else
                    None)
        |> Seq.cast<Option<int * int>>
        |> Seq.filter Option.isSome
        |> Seq.map (Option.defaultValue (-1, -1))
        |> Seq.map (expandBasin array)
        |> Seq.sortDescending
        |> Seq.take 3
        |> Seq.fold (fun a b -> a * b) 1
        |> string
