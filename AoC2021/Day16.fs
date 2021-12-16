namespace AoC2021

module Day16 =
    type Packet =
        | LiteralValue of version: int64 * typeID: int64 * value: int64 * length: int64
        | Operator of version: int64 * typeID: int64 * lengthTypeID: int64 * subPackets: Packet [] * length: int64

    let hexToBin c =
        match c with
        | '0' -> "0000"
        | '1' -> "0001"
        | '2' -> "0010"
        | '3' -> "0011"
        | '4' -> "0100"
        | '5' -> "0101"
        | '6' -> "0110"
        | '7' -> "0111"
        | '8' -> "1000"
        | '9' -> "1001"
        | 'A' -> "1010"
        | 'B' -> "1011"
        | 'C' -> "1100"
        | 'D' -> "1101"
        | 'E' -> "1110"
        | 'F' -> "1111"

    let binStringToDec b = System.Convert.ToInt64(b, 2)

    let binArrayToDec (b: char []) =
        System.Convert.ToInt64(System.String.Concat b, 2)

    let rec parseBitChunks (acc: char []) length (chunks: char [] []) =
        match chunks with
        | [||] -> (acc, length)
        | _ ->
            let chunk = Array.head chunks

            match chunk.[0] with
            | '1' -> parseBitChunks (Array.append acc chunk.[1..]) (length + 5L) (Array.tail chunks)
            | '0' -> (Array.append acc chunk.[1..], length + 5L)

    let packetLength (packet: Packet) : int64 =
        match packet with
        | LiteralValue (_, _, _, length) -> length
        | Operator (_, _, _, _, length) -> length

    let rec identifyPacket (binArray: char []) =
        let rec sequentialPackets (acc: Packet []) (binArray: char []) : Packet [] =
            match binArray.Length > 6 with
            | false -> acc
            | true ->
                let packet = identifyPacket binArray

                match packet with
                | LiteralValue (_, _, _, length) ->
                    sequentialPackets (Array.append acc [| packet |]) (binArray.[int length..])
                | Operator (_, _, _, _, length) ->
                    sequentialPackets (Array.append acc [| packet |]) (binArray.[int length..])

        let rec sequentialPacketsCount (count: int64) (acc: Packet []) (binArray: char []) : Packet [] =
            match count with
            | 0L -> acc
            | _ ->
                let packet = identifyPacket binArray

                match packet with
                | LiteralValue (_, _, _, length) ->
                    sequentialPacketsCount (count - 1L) (Array.append acc [| packet |]) (binArray.[int length..])
                | Operator (_, _, _, _, length) ->
                    sequentialPacketsCount (count - 1L) (Array.append acc [| packet |]) (binArray.[int length..])

        let packetVersion = binArray.[0..2] |> binArrayToDec

        let packetTypeID = binArray.[3..5] |> binArrayToDec

        match packetTypeID with
        | 4L ->
            let (value, length) =
                binArray.[6..]
                |> Array.chunkBySize 5
                |> parseBitChunks [||] 6L

            let value' = value |> binArrayToDec

            LiteralValue(packetVersion, packetTypeID, value', length)
        | _ ->
            let packetLengthTypeID = binArray.[6] |> string |> binStringToDec

            match packetLengthTypeID with
            | 0L ->
                //the next 15 bits are a number that represents the total length in bits of the sub-packets contained by this packet.
                let subPacketsLength = binArray.[7..21] |> binArrayToDec

                let subPackets =
                    binArray.[22..(22 + int subPacketsLength - 1)]
                    |> sequentialPackets [||]

                Operator(packetVersion, packetTypeID, packetLengthTypeID, subPackets, 7L + 15L + subPacketsLength)
            | 1L ->
                //the next 11 bits are a number that represents the number of sub-packets immediately contained by this packet.
                let subPacketsCount = binArray.[7..17] |> binArrayToDec

                let subPackets =
                    binArray.[18..]
                    |> sequentialPacketsCount subPacketsCount [||]

                let subPacketsLength =
                    subPackets
                    |> Array.map (fun p -> packetLength p)
                    |> Array.sum

                Operator(packetVersion, packetTypeID, packetLengthTypeID, subPackets, 7L + 11L + subPacketsLength)

    let rec sumVersionNumbers (packet: Packet) : int64 =
        match packet with
        | LiteralValue (version, _, _, _) -> version
        | Operator (version, _, _, packets, _) -> Array.fold (fun acc p -> sumVersionNumbers p + acc) version packets

    let part1 (input: string) : string =
        let binArray =
            input.Trim()
            |> Array.ofSeq
            |> Array.map hexToBin
            |> Seq.concat
            |> Array.ofSeq
            |> identifyPacket
            |> sumVersionNumbers
            |> string

        binArray

    let rec performCalculations (packet: Packet) : int64 =
        match packet with
        | LiteralValue (_, _, value, _) -> value
        | Operator (_, typeID, _, packets, _) ->
            let calculatedPackets = Array.map performCalculations packets

            match typeID with
            | 0L -> // sum
                calculatedPackets |> Array.sum
            | 1L -> // product
                calculatedPackets
                |> Array.fold (fun s n -> s * n) 1L
            | 2L -> // min
                calculatedPackets |> Array.min
            | 3L -> // max
                calculatedPackets |> Array.max
            | 5L -> // gt
                if calculatedPackets.[0] > calculatedPackets.[1] then
                    1L
                else
                    0L
            | 6L -> // lt
                if calculatedPackets.[0] < calculatedPackets.[1] then
                    1L
                else
                    0L
            | 7L -> // eq
                if calculatedPackets.[0] = calculatedPackets.[1] then
                    1L
                else
                    0L

    let part2 (input: string) : string =
        let binArray =
            input.Trim()
            |> Array.ofSeq
            |> Array.map hexToBin
            |> Seq.concat
            |> Array.ofSeq
            |> identifyPacket
            |> performCalculations
            |> string

        binArray
