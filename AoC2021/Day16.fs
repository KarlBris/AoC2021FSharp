namespace AoC2021

open Utils

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
            | '1' -> parseBitChunks (Array.append acc chunk.[1..]) (length + 5) (Array.tail chunks)
            | '0' -> (Array.append acc chunk.[1..], length + 5)

    let packetLength (packet: Packet) : int64 =
        match packet with
        | LiteralValue (_, _, _, length) -> length
        | Operator (_, _, _, _, length) -> length

    let rec identifyPacket (binArray: char []) =
        let rec sequentialPackets (binArray: char []) (acc: Packet []) : Packet [] =
            match binArray.Length > 6 with
            | false -> acc
            | true ->
                let packet = identifyPacket binArray

                match packet with
                | LiteralValue (_, _, _, length) ->
                    sequentialPackets (binArray.[int length..]) (Array.append acc [| packet |])
                | Operator (_, _, _, _, length) ->
                    sequentialPackets (binArray.[int length..]) (Array.append acc [| packet |])

        let rec sequentialPacketsCount (count: int64) (binArray: char []) (acc: Packet []) : Packet [] =
            match count with
            | 0L -> acc
            | _ ->
                if count = 959 then
                    "sssssssssssssssssssssssssssssssssssssssss"
                else
                    "a"

                let packet = identifyPacket binArray

                match packet with
                | LiteralValue (_, _, _, length) ->
                    sequentialPacketsCount (count - 1L) (binArray.[int length..]) (Array.append acc [| packet |])
                | Operator (_, _, _, _, length) ->
                    sequentialPacketsCount (count - 1L) (binArray.[int length..]) (Array.append acc [| packet |])

        let packetVersionTemp = binArray.[0..2]

        if packetVersionTemp.Length < 3 then
            0
        else
            1

        let packetVersion = binArray.[0..2] |> binArrayToDec

        let packetTypeID = binArray.[3..5] |> binArrayToDec

        match packetTypeID with
        | 4L ->
            let (value, length) =
                binArray.[6..]
                |> Array.chunkBySize 5
                |> parseBitChunks [||] 6

            let value' = value |> binArrayToDec

            LiteralValue(packetVersion, packetTypeID, value', length)
        | _ ->
            let packetLengthTypeID = binArray.[6] |> string |> binStringToDec

            match packetLengthTypeID with
            | 0L ->
                //the next 15 bits are a number that represents the total length in bits of the sub-packets contained by this packet.

                let subPacketsLength = binArray.[7..21] |> binArrayToDec

                let subPacketsBin =
                    binArray.[22..(22 + int subPacketsLength - 1)]

                let subPackets = sequentialPackets subPacketsBin [||]

                printfn "0: subpackets are of stated length %d" subPacketsLength
                Operator(packetVersion, packetTypeID, packetLengthTypeID, subPackets, 7L + 15L + subPacketsLength)
            | 1L ->
                //the next 11 bits are a number that represents the number of sub-packets immediately contained by this packet.
                let subPacketsCount = binArray.[7..17] |> binArrayToDec
                printfn "%d subpackets" subPacketsCount
                let subPacketsBin = binArray.[18..]

                let subPackets =
                    sequentialPacketsCount subPacketsCount subPacketsBin [||]

                let subPacketsLength =
                    subPackets
                    |> Array.map (fun p -> packetLength p)
                    |> Array.sum

                printfn "1: subpackets are of total length %d" subPacketsLength
                Operator(packetVersion, packetTypeID, packetLengthTypeID, subPackets, 7L + 15L + subPacketsLength)

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

    let part2 (input: string) : string = input
