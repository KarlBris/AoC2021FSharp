namespace AoC2021

open Utils

module Day23 =

    type RoomId =
        | A
        | B
        | C
        | D

    type Room = RoomId * (char list) // done

    type Corridor = char []

    type Position =
        | Rm of RoomId
        | Cr of int

    let roomList (roomSize: int) ((_, room): Room) : char List = // done
        let header =
            List.replicate (roomSize - room.Length) '.'

        header @ room

    type GameState = (Room * Room * Room * Room * Corridor) // done

    let printState (roomSize: int) (state: GameState) = // done

        let (rA, rB, rC, rD, c) = state

        let aList = roomList roomSize rA
        let bList = roomList roomSize rB
        let cList = roomList roomSize rC
        let dList = roomList roomSize rD

        printfn "#############"
        printfn "#%s#" (System.String.Concat c)
        printfn "###%c#%c#%c#%c###" aList.[0] bList.[0] cList.[0] dList.[0]

        [ 1 .. (roomSize - 1) ]
        |> List.iter (fun i -> printfn "  #%c#%c#%c#%c#" aList.[i] bList.[i] cList.[i] dList.[i])

        printfn "  #########\n"

    let disallowedCorridorPositions = [ 2; 4; 6; 8 ] // done

    let getRoomId (c: char) : RoomId = // done
        match c with
        | 'A' -> A
        | 'B' -> B
        | 'C' -> C
        | _ -> D

    let makeChar (roomId: RoomId) : char = // done
        match roomId with
        | A -> 'A'
        | B -> 'B'
        | C -> 'C'
        | D -> 'D'

    let corridorPosOfRoom room = // done
        match room with
        | A -> 2
        | B -> 4
        | C -> 6
        | D -> 8

    let stepCost (c: char) : int = // done
        match c with
        | 'A' -> 1
        | 'B' -> 10
        | 'C' -> 100
        | _ -> 1000

    let posFromRoom (room: Room) : Position list = // gets a position from this room that is eligible for move // done
        let (rId, roomList) = room

        match roomList with
        | [] -> []
        | c :: cs ->
            if (List.forall ((=) (makeChar rId)) cs)
               && (c = (makeChar rId)) then
                []
            else
                [ Rm rId ]

    let removeFromRoom (room: Room) : Room = // done
        let (rId, roomList) = room

        match roomList with
        | [] -> (rId, [])
        | (r :: rs) -> (rId, rs)

    let possFromCorridor (corridor: Corridor) : Position list = // done
        corridor
        |> Array.indexed
        |> Array.filter (fun (_, c) -> c <> '.')
        |> Array.map (fun (i, c) -> Cr i)
        |> List.ofArray

    let canRoomBeEntered (roomSize: int) (c: char) (room: Room) : bool = // done
        let (rId, roomList) = room

        (rId = getRoomId c)
        && (List.forall (fun x -> x = c) roomList)
        && (roomList.Length < roomSize)

    let availableSpaceInCorridor (disallowEntryways: bool) (entryPoint: int) (corridor: Corridor) : int list = // done
        let (leftOfEntry, rightOfEntry) =
            corridor
            |> List.ofArray
            |> List.indexed
            |> List.partition (fun (i, _) -> i < entryPoint)

        let newRight =
            rightOfEntry
            |> List.tail
            |> List.takeWhile (fun (_, c) -> c = '.')

        let newLeft =
            leftOfEntry
            |> List.rev
            |> List.takeWhile (fun (_, c) -> c = '.')

        let foo =
            newLeft
            |> List.append newRight
            |> List.map (fun (i, c) -> i)
            |> List.sort

        if disallowEntryways then
            foo |> List.except disallowedCorridorPositions
        else
            foo

    let possibleMovers (currentState: GameState) : Position list = // done
        let (rA, rB, rC, rD, c) = currentState

        (posFromRoom rA)
        @ (posFromRoom rB)
          @ (posFromRoom rC)
            @ (posFromRoom rD) @ (possFromCorridor c)

    let fooFun (roomSize: int) (r: RoomId) (rA: Room) (rB: Room) (rC: Room) (rD: Room) : int =
        match r with
        | A -> roomSize - (rA |> snd |> List.length) + 1
        | B -> roomSize - (rB |> snd |> List.length) + 1
        | C -> roomSize - (rC |> snd |> List.length) + 1
        | D -> roomSize - (rD |> snd |> List.length) + 1

    let getFromRoom (r: RoomId) (rA: Room) (rB: Room) (rC: Room) (rD: Room) : char =
        match r with
        | A -> (rA |> snd |> List.head)
        | B -> (rB |> snd |> List.head)
        | C -> (rC |> snd |> List.head)
        | D -> (rD |> snd |> List.head)

    let moveToCorridorPos
        (roomSize: int)
        (pos: Position)
        (corOutPos: int)
        (targetCorPos: int)
        (currentState: GameState)
        : GameState * int =
        let (rA, rB, rC, rD, cor) = currentState

        match pos with
        | Rm r ->
            let cor2 = Array.copy cor
            let stepsToCorridor = fooFun roomSize r rA rB rC rD
            let c = getFromRoom r rA rB rC rD
            Array.set cor2 targetCorPos c // funky mutation. not a fan.

            let newGameState =
                match r with
                | A -> (removeFromRoom rA, rB, rC, rD, cor2)
                | B -> (rA, removeFromRoom rB, rC, rD, cor2)
                | C -> (rA, rB, removeFromRoom rC, rD, cor2)
                | D -> (rA, rB, rC, removeFromRoom rD, cor2)

            let steps =
                stepsToCorridor + abs (corOutPos - targetCorPos)

            (newGameState, steps * stepCost c)
        | _ -> failwith "Tried to move to a corridor pos from another corridor pos..."

    let addToRoom (c: char) (room: Room) : Room = // done
        let (roomid, roomList) = room
        (roomid, c :: roomList)

    let possibleMoves (roomSize: int) (currentState: GameState) (position: Position) : ((GameState * int) * bool) list =
        let (rA, rB, rC, rD, cor) = currentState
        let cor2 = Array.copy cor

        match position with
        | Rm r ->
            let corridorOut = corridorPosOfRoom r

            let availableCorridorPositions =
                availableSpaceInCorridor true corridorOut cor2

            availableCorridorPositions
            |> List.map (fun p -> (moveToCorridorPos roomSize position corridorOut p currentState), false)
        | Cr cPos ->
            let c = cor2.[cPos]

            let targetRoom =
                if c = 'A' then rA
                else if c = 'B' then rB
                else if c = 'C' then rC
                else rD

            let availableCorridorPositions = availableSpaceInCorridor false cPos cor2

            let targetRoomCorrPos = corridorPosOfRoom (getRoomId c)

            if List.contains targetRoomCorrPos availableCorridorPositions then

                let targetRoomId = getRoomId c

                match canRoomBeEntered roomSize c targetRoom with
                | true ->
                    let stepsFromCorridor =
                        (fooFun roomSize targetRoomId rA rB rC rD) - 1

                    let steps =
                        stepsFromCorridor
                        + abs (cPos - (corridorPosOfRoom targetRoomId))

                    Array.set cor2 cPos '.' // funky mutation. not a fan.

                    let newGameState =
                        match targetRoomId with
                        | A -> (addToRoom c rA, rB, rC, rD, cor2)
                        | B -> (rA, addToRoom c rB, rC, rD, cor2)
                        | C -> (rA, rB, addToRoom c rC, rD, cor2)
                        | D -> (rA, rB, rC, addToRoom c rD, cor2)

                    [ (newGameState, steps * stepCost c), true ]
                | false -> []
            else
                []

    let parseInput (roomSize: int) (input: char [] []) : GameState = // done

        let room1 =
            (A, Array.toList input.[3].[2..(1 + roomSize)])

        let room2 =
            (B, Array.toList input.[5].[2..(1 + roomSize)])

        let room3 =
            (C, Array.toList input.[7].[2..(1 + roomSize)])

        let room4 =
            (D, Array.toList input.[9].[2..(1 + roomSize)])

        (room1, room2, room3, room4, Array.replicate 11 '.')

    let isRoomCorrect (roomSize: int) (room: Room) : bool = // done
        let (roomId, spaces) = room

        spaces.Length = roomSize
        && (List.forall ((=) (makeChar roomId)) spaces)

    let isEndState (roomSize: int) (state: GameState) : bool = // done
        let (rA, rB, rC, rD, cor) = state

        Array.forall ((=) '.') cor
        && isRoomCorrect roomSize rA
        && isRoomCorrect roomSize rB
        && isRoomCorrect roomSize rC
        && isRoomCorrect roomSize rD

    let rec move (roomSize: int) (scores: int list) (acc: int) (state: GameState) : Option<int> =

        if isEndState roomSize state then
            Some acc
        else
            let newStatesTemp =
                state
                |> possibleMovers
                |> List.map (possibleMoves roomSize state)
                |> List.concat

            let (prio, normal) = newStatesTemp |> List.partition snd

            let newStates =
                if prio.Length > 0 then
                    prio
                    |> List.map (fun (a, b) -> a)
                    |> List.sortBy snd
                else
                    normal
                    |> List.map (fun (a, b) -> a)
                    |> List.sortBy snd



            if newStates.Length = 0 then
                None
            else
                let asd =
                    newStates
                    |> List.fold
                        (fun s (newState, cost) ->
                            if acc < s then
                                match move roomSize scores (acc + cost) newState with
                                | None -> s
                                | Some v -> min v s
                            else
                                s)
                        1000000

                Some asd

    let part1 (input: string) : string =
        let roomSize = 2

        let transposed =
            input
            |> lines
            |> Array.map
                (fun l ->
                    if l.Length = 13 then
                        Array.ofSeq l
                    else
                        Array.ofSeq (l + "  "))
            |> Array.transpose

        let state = parseInput roomSize transposed

        let foo = move roomSize [] 0 state

        foo |> Option.get |> string

    let part2 (input: string) : string =
        let roomSize = 4

        let inputLines = input |> lines

        let inputLines2 =
            Array.append inputLines.[0..2] (Array.append [| "  #D#C#B#A#"; "  #D#B#A#C#" |] inputLines.[3..4])

        inputLines2
        |> Array.map
            (fun l ->
                if l.Length = 13 then
                    Array.ofSeq l
                else
                    Array.ofSeq (l + "  "))
        |> Array.transpose
        |> parseInput roomSize
        |> move roomSize [] 0
        |> Option.get
        |> string
