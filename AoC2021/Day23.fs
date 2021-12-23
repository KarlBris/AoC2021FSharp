namespace AoC2021

open Utils

module Day23 =

    type RoomPos =
        | Front
        | Back

    type RoomId =
        | A
        | B
        | C
        | D

    type Position =
        | Room of (RoomId * char * RoomPos)
        | Corridor of (char * int)

    type GameState = (Position list * Position list * Position list * Position list * char [])

    let disallowedCorridorPositions = [ 2; 4; 6; 8 ]

    let corridorPosOfRoom room =
        match room with
        | A -> 2
        | B -> 4
        | C -> 6
        | D -> 8

    let stepCost (c: char) : int =
        match c with
        | 'A' -> 1
        | 'B' -> 10
        | 'C' -> 100
        | _ -> 1000

    let posFromRoom (room: Position list) : Position list =
        match room with
        | [] -> []
        | (r :: rs) -> [ r ]

    let removeFromRoom (room: Position list) : Position list =
        match room with
        | [] -> []
        | (r :: rs) -> rs

    let possFromCorridor (corridor: char []) : Position list =
        corridor
        |> Array.indexed
        |> Array.filter (fun (_, c) -> c <> '.')
        |> Array.map (fun (i, c) -> Corridor(c, i))
        |> List.ofArray

    let canRoomBeEntered (room: Position list) : Option<RoomPos> =
        match room.Length with
        | 0 -> Some Back
        | 1 -> Some Front
        | _ -> None

    let availableSpaceInCorridor (entryPoint: int) (corridor: char []) : int list =
        let (leftOfEntry, rightOfEntry) =
            corridor
            |> List.ofArray
            |> List.indexed
            |> List.partition (fun (i, _) -> i < entryPoint)

        let newRight =
            rightOfEntry
            |> List.takeWhile (fun (_, c) -> c = '.')

        let newLeft =
            leftOfEntry
            |> List.rev
            |> List.takeWhile (fun (_, c) -> c = '.')

        newLeft
        |> List.append newRight
        |> List.map (fun (i, c) -> i)
        |> List.except disallowedCorridorPositions

    let possibleMovers (currentState: GameState) : Position list =
        let (rA, rB, rC, rD, c) = currentState
        // TODO: don't give the dudes that are already in their correct place!
        (posFromRoom rA)
        @ (posFromRoom rB)
          @ (posFromRoom rC)
            @ (posFromRoom rD) @ (possFromCorridor c)

    let moveToCorridorPos
        (pos: Position)
        (corOutPos: int)
        (targetCorPos: int)
        (currentState: GameState)
        : GameState * int =
        let (rA, rB, rC, rD, cor) = currentState

        match pos with
        | Room (r, c, rPos) ->
            let cor2 = Array.copy cor
            let stepsToCorridor = if rPos = Front then 1 else 2
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

    let getRoomId (c: char) : RoomId =
        match c with
        | 'A' -> A
        | 'B' -> B
        | 'C' -> C
        | _ -> D

    let addToRoom (roomId: RoomId) (c: char) (room: Position list) : Position list =
        match room with
        | [] -> [ Room(roomId, c, Back) ]
        | _ -> (Room(roomId, c, Front)) :: room

    let possibleMoves (currentState: GameState) (position: Position) : (GameState * int) list =
        let (rA, rB, rC, rD, cor) = currentState
        let cor2 = Array.copy cor

        match position with
        | Room (r, c, rPos) ->
            let corridorOut = corridorPosOfRoom r

            let availableCorridorPositions =
                availableSpaceInCorridor corridorOut cor2

            availableCorridorPositions
            |> List.map (fun p -> moveToCorridorPos position corridorOut p currentState)
        | Corridor (c, cPos) ->
            let targetRoom =
                if c = 'A' then rA
                else if c = 'B' then rB
                else if c = 'C' then rC
                else rD

            let targetRoomId = getRoomId c

            match canRoomBeEntered targetRoom with
            | Some rPos ->
                let stepsFromCorridor = if rPos = Front then 1 else 2

                let steps =
                    stepsFromCorridor
                    + abs (cPos - (corridorPosOfRoom targetRoomId))

                Array.set cor2 cPos '.' // funky mutation. not a fan.

                let newGameState =
                    match targetRoomId with
                    | A -> (addToRoom A c rA, rB, rC, rD, cor2)
                    | B -> (rA, addToRoom B c rB, rC, rD, cor2)
                    | C -> (rA, rB, addToRoom C c rC, rD, cor2)
                    | D -> (rA, rB, rC, addToRoom D c rD, cor2)

                [ (newGameState, steps * stepCost c) ]
            | None -> []

    let parseInput (input: string) : GameState =
        let room1 =
            [ Room(A, input.[31], Front)
              Room(A, input.[45], Back) ]

        let room2 =
            [ Room(B, input.[33], Front)
              Room(B, input.[47], Back) ]

        let room3 =
            [ Room(C, input.[35], Front)
              Room(C, input.[49], Back) ]

        let room4 =
            [ Room(D, input.[37], Front)
              Room(D, input.[51], Back) ]

        (room1, room2, room3, room4, Array.replicate 11 '.')

    let part1 (input: string) : string =
        let parsed = parseInput input
        let (rA, rB, rC, rD, c) = parsed

        let movers = possibleMovers parsed

        let newStates =
            movers
            |> List.map (possibleMoves parsed)
            |> List.concat

        input

    let part2 (input: string) : string = input
