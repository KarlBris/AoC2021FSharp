namespace AoC2021

open Utils

module Day19 =

    type Coord = int * int * int

    type Entry =
        | Beacon
        | Scanner

    let minMatchingBeacons = 12

    let processInput (input: string) : Map<Coord, Entry> =
        let lines = input |> lines

        lines
        |> Array.tail
        |> Array.map
            (fun line ->
                let coords = line.Split(',') |> Array.map int
                ((coords.[0], coords.[1], coords.[2]), Beacon))
        |> Map.ofArray
        |> Map.add (0, 0, 0) Scanner

    let axisPermutations (coord: Coord) : Coord [] =
        let (x, y, z) = coord

        [| (x, y, z)
           (x, z, y)
           (y, x, z)
           (y, z, x)
           (z, x, y)
           (z, y, x) |]

    let signPermutations (coord: Coord) : Coord [] =
        let (x, y, z) = coord

        [| (x, y, z)
           (x, y, -z)
           (x, -y, z)
           (x, -y, -z)
           (-x, y, z)
           (-x, y, -z)
           (-x, -y, z)
           (-x, -y, -z) |]

    let bothPermutations (coord: Coord) : Coord [] =
        coord
        |> axisPermutations
        |> Array.map signPermutations
        |> Array.concat

    let addCoords (coord1: Coord) (coord2: Coord) : Coord =
        let (x1, y1, z1) = coord1
        let (x2, y2, z2) = coord2
        (x1 + x2, y1 + y2, z1 + z2)

    let rec testMatchings (globalMap: Map<Coord, Entry>) (testMaps: Map<Coord, Entry> []) =
        match testMaps with
        | [||] -> None
        | _ ->
            let testCoords =
                Array.head testMaps
                |> Map.toArray
                |> Array.map fst

            let (highestFrequencyDiff, numberOfMatches) =
                Map.toArray globalMap
                |> Array.map fst
                |> Array.map
                    (fun (gX, gY, gZ) -> Array.map (fun (tX, tY, tZ) -> (gX - tX, gY - tY, gZ - tZ)) testCoords)
                |> Array.concat
                |> Array.countBy id
                |> Array.sortByDescending snd
                |> Array.head

            if numberOfMatches >= minMatchingBeacons then

                let transformedTestProtoMap =
                    testCoords
                    |> Array.map
                        (fun coord ->
                            let entryType =
                                if coord = (0, 0, 0) then
                                    Scanner
                                else
                                    Beacon

                            (addCoords highestFrequencyDiff coord, entryType))

                Array.fold (fun map (k, v) -> Map.add k v map) globalMap transformedTestProtoMap
                |> Some
            else
                testMatchings globalMap (Array.tail testMaps)

    let matching (testMap: Map<Coord, Entry>) (globalMap: Map<Coord, Entry>) : Map<Coord, Entry> option =
        testMap
        |> Map.toArray
        |> Array.map (fun (k, v) -> bothPermutations k |> Array.map (fun p -> (p, v)))
        |> Array.transpose
        |> Array.map (fun x -> Map.ofArray x)
        |> testMatchings globalMap

    let rec matchBeacons (beaconLists: Map<Coord, Entry> []) (globalMap: Map<Coord, Entry>) : Map<Coord, Entry> =
        match beaconLists with
        | [||] -> globalMap
        | _ ->
            match (matching (Array.head beaconLists) globalMap) with
            | None -> matchBeacons (Array.append (Array.tail beaconLists) [| Array.head beaconLists |]) globalMap
            | Some newGlobalMap -> matchBeacons (Array.tail beaconLists) newGlobalMap

    let part1 (input: string) : string =
        let parsed =
            input.Split("\n\n") |> Array.map processInput

        let updatedMap =
            parsed
            |> Array.head
            |> matchBeacons (Array.tail parsed)
            |> Map.filter (fun _ v -> v <> Scanner)

        updatedMap.Count |> string

    let manhattanDist ((x1, y1, z1): Coord) ((x2, y2, z2): Coord) : int =
        (abs (x1 - x2))
        + (abs (y1 - y2))
        + (abs (z1 - z2))

    let part2 (input: string) : string =
        let parsed =
            input.Split("\n\n") |> Array.map processInput

        let updatedMap =
            parsed
            |> Array.head
            |> matchBeacons (Array.tail parsed)
            |> Map.filter (fun _ v -> v = Scanner)
            |> Map.toArray
            |> Array.map fst

        Array.allPairs updatedMap updatedMap
        |> Array.map (fun (c1, c2) -> manhattanDist c1 c2)
        |> Array.sortDescending
        |> Array.head
        |> string
