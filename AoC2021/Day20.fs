namespace AoC2021

open Utils

module Day20 =
    let parseInput (input: string) : (bool []) * (bool [,]) =
        let (algorithmString, imageString) = input.Split("\n\n") |> twoArrayToTuple

        let algorithm =
            algorithmString
            |> Array.ofSeq
            |> Array.map ((=) '#')

        let image =
            imageString
            |> lines
            |> Array.map (fun l -> l |> Array.ofSeq |> Array.map ((=) '#'))
            |> array2D

        (algorithm, image)

    let createKernel (x: int) (y: int) (length1: int) (length2: int) : Option<int * int> [] =
        Array.allPairs [| x - 1 .. x + 1 |] [| y - 1 .. y + 1 |]
        |> Array.map
            (fun (x, y) ->
                if x > 0 && x <= length1 && y > 0 && y <= length2 then
                    Some(x, y)
                else
                    None)

    let applyKernel (background: char) (image: bool [,]) (algorithm: bool []) (kernel: Option<int * int> []) : bool =

        let binString =
            kernel
            |> Array.map
                (fun op ->
                    match op with
                    | None -> background
                    | Some (x, y) ->
                        if image.[x - 1, y - 1] then
                            '1'
                        else
                            '0')
            |> System.String.Concat

        System.Convert.ToInt32(binString, 2)
        |> Array.get algorithm

    let enhance (algorithm: bool []) ((image, background): (bool [,]) * (bool)) : (bool [,]) * bool =
        let length1 = Array2D.length1 image
        let length2 = Array2D.length2 image

        let newImage =
            Array2D.create (length1 + 2) (length2 + 2) false
            |> Array2D.mapi
                (fun x y _ ->
                    createKernel x y length1 length2
                    |> applyKernel (if background then '1' else '0') image algorithm)

        let newBackground =
            (if background then 511 else 0)
            |> Array.get algorithm

        (newImage, newBackground)

    let rec enhanceIter
        (times: int)
        (algorithm: bool [])
        ((image, background): (bool [,]) * (bool))
        : (bool [,]) * bool =
        match times with
        | 0 -> (image, background)
        | _ ->
            let newIB = enhance algorithm (image, background)
            enhanceIter (times - 1) algorithm newIB

    let part1 (input: string) : string =
        let (algorithm, image) = parseInput input

        (image, false)
        |> enhanceIter 2 algorithm
        |> fst
        |> Seq.cast<bool>
        |> Array.ofSeq
        |> Array.filter id
        |> Array.length
        |> string

    let part2 (input: string) : string =
        let (algorithm, image) = parseInput input

        (image, false)
        |> enhanceIter 50 algorithm
        |> fst
        |> Seq.cast<bool>
        |> Array.ofSeq
        |> Array.filter id
        |> Array.length
        |> string
