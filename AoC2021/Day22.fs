namespace AoC2021

open Utils

module Day22 =
    type Cube = (int * int) * (int * int) * (int * int)
    type RebootStep = (bool * Cube)

    let processLine (input: string) : RebootStep =
        let (stateString, insString) = words input |> twoArrayToTuple
        let state = stateString = "on"

        let foo =
            insString.Split(',')
            |> Array.map
                (fun s ->
                    let (_, nums) = s.Split('=') |> twoArrayToTuple

                    nums.Split("..")
                    |> Array.map int
                    |> twoArrayToTuple)

        (state, (foo.[0], foo.[1], foo.[2]))

    let overlapCube (cube1: Cube) (cube2: Cube) : Option<Cube> =
        let ((minX1, maxX1), (minY1, maxY1), (minZ1, maxZ1)) = cube1
        let ((minX2, maxX2), (minY2, maxY2), (minZ2, maxZ2)) = cube2

        if maxX1 >= minX2 && minX1 <= maxX2 then // x overlap
            let newMinX = max minX1 minX2
            let newMaxX = min maxX1 maxX2

            if maxY1 >= minY2 && minY1 <= maxY2 then // y overlap
                let newMinY = max minY1 minY2
                let newMaxY = min maxY1 maxY2

                if maxZ1 >= minZ2 && minZ1 <= maxZ2 then // z overlap
                    let newMinZ = max minZ1 minZ2
                    let newMaxZ = min maxZ1 maxZ2

                    Some((newMinX, newMaxX), (newMinY, newMaxY), (newMinZ, newMaxZ))
                else
                    None
            else
                None
        else
            None

    let cubeSize (cube: Cube) : int64 =
        let ((minX, maxX), (minY, maxY), (minZ, maxZ)) = cube

        (int64 (maxX) - int64 (minX) + 1L)
        * (int64 (maxY) - int64 (minY) + 1L)
        * (int64 (maxZ) - int64 (minZ) + 1L)

    let complementCubes (cube: Cube) : Cube [] =
        let ((minX, maxX), (minY, maxY), (minZ, maxZ)) = cube
        let intMax = System.Int32.MaxValue
        let intMin = System.Int32.MinValue

        let x = (minX, maxX)
        let y = (minY, maxY)
        let z = (minZ, maxZ)

        let xPosInf = (maxX + 1, intMax)
        let xNegInf = (intMin, minX - 1)
        let yPosInf = (maxY + 1, intMax)
        let yNegInf = (intMin, minY - 1)
        let zPosInf = (maxZ + 1, intMax)
        let zNegInf = (intMin, minZ - 1)

        [| (xPosInf, y, z)
           (xNegInf, y, z)
           (x, yPosInf, z)
           (x, yNegInf, z)
           (x, y, zPosInf)
           (x, y, zNegInf)
           (xPosInf, yPosInf, z)
           (xNegInf, yPosInf, z)
           (xPosInf, yNegInf, z)
           (xNegInf, yNegInf, z)
           (xPosInf, y, zPosInf)
           (xNegInf, y, zPosInf)
           (xPosInf, y, zNegInf)
           (xNegInf, y, zNegInf)
           (xPosInf, yPosInf, zPosInf)
           (xPosInf, yPosInf, zNegInf)
           (xPosInf, yNegInf, zPosInf)
           (xPosInf, yNegInf, zNegInf)
           (xNegInf, yPosInf, zPosInf)
           (xNegInf, yPosInf, zNegInf)
           (xNegInf, yNegInf, zPosInf)
           (xNegInf, yNegInf, zNegInf)
           (x, yPosInf, zPosInf)
           (x, yNegInf, zPosInf)
           (x, yPosInf, zNegInf)
           (x, yNegInf, zNegInf) |]

    let subtract (cube1: Cube) (cube2: Cube) : Cube [] =
        // cube1 minus cube2
        cube2
        |> complementCubes
        |> Array.map (fun c -> overlapCube cube1 c)
        |> Array.filter Option.isSome
        |> Array.map Option.get

    let union (cube1: Cube) (cube2: Cube) : Cube [] =
        // cube1 and partitioned cube2 minus the overlap
        Array.append [| cube1 |] (subtract cube2 cube1)

    let rec subtractCubeFromArray (add: bool) (cube: Cube) (cubes: Cube []) : Cube [] =
        match cubes with
        | [||] -> [| cube |]
        | _ ->
            let (overlapping, nonOverlapping) =
                cubes
                |> Array.partition (fun c -> overlapCube c cube |> Option.isSome)

            let subtractedOverlapping =
                overlapping
                |> Array.map (fun oc -> subtract oc cube)
                |> Array.concat

            if add then
                Array.append [| cube |] (Array.append subtractedOverlapping nonOverlapping)
            else
                Array.append subtractedOverlapping nonOverlapping

    let cubesSize (cubes: Cube []) : int64 =
        cubes |> Array.map cubeSize |> Array.sum

    let rec applyCubes (cubes: Cube []) (steps: RebootStep []) : Cube [] =
        match steps with
        | [||] -> cubes
        | _ ->
            let (add, cube) = Array.head steps
            let restSteps = Array.tail steps

            applyCubes (subtractCubeFromArray add cube cubes) restSteps

    let part1 (input: string) : string =
        let inclusionCube = ((-50, 50), (-50, 50), (-50, 50))

        input
        |> lines
        |> Array.map processLine
        |> applyCubes [||]
        |> Array.filter (fun c -> overlapCube c inclusionCube |> Option.isSome)
        |> cubesSize
        |> string

    let part2 (input: string) : string =
        input
        |> lines
        |> Array.map processLine
        |> applyCubes [||]
        |> cubesSize
        |> string
