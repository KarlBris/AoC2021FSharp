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

    let roomList (room: Position List) : char List =
        let roomChars =
            room
            |> List.map
                (fun p ->
                    match p with
                    | Room (a, b, c) -> b
                    | Corridor _ -> failwith "corridor in room")

        match room.Length with
        | 0 -> [ '.'; '.' ]
        | 1 -> '.' :: roomChars
        | 2 -> roomChars


    type GameState = (Position list * Position list * Position list * Position list * char [])

    let printState (state: GameState) =

        let (rA, rB, rC, rD, c) = state

        let aList = roomList rA
        let bList = roomList rB
        let cList = roomList rC
        let dList = roomList rD

        printfn "#############"
        printfn "#%s#" (System.String.Concat c)
        printfn "###%c#%c#%c#%c###" aList.[0] bList.[0] cList.[0] dList.[0]
        printfn "  #%c#%c#%c#%c#" aList.[1] bList.[1] cList.[1] dList.[1]
        printfn "  #########\n"

    let disallowedCorridorPositions = [ 2; 4; 6; 8 ]

    let getRoomId (c: char) : RoomId =
        match c with
        | 'A' -> A
        | 'B' -> B
        | 'C' -> C
        | _ -> D

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

    let posFromRoom (room: Position list) (rID: RoomId) : Position list =
        match room with
        | [] -> []
        | (r :: rs) ->
            match r with
            | Room (roomId, c, p) ->
                match p with
                | Back ->
                    if getRoomId c = roomId then
                        []
                    else
                        [ r ]
                | Front ->
                    if getRoomId c = roomId then
                        match List.head rs with
                        | Room (id2, c2, p2) ->
                            match p2 with
                            | Back ->
                                if getRoomId c2 = roomId then
                                    []
                                else
                                    [ r ]
                            | Front -> failwith "front room behind front!"
                        | Corridor _ -> failwith "corridor pos in room"
                    else
                        [ r ]
            | Corridor _ -> failwith "corridor pos in room"

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
        | 1 ->
            let inhabitant = List.head room

            match inhabitant with
            | Room (a, b, c) ->
                if getRoomId b = a then
                    Some Front
                else
                    None
            | Corridor _ -> failwith "corridor pos in room"
        | _ -> None

    let availableSpaceInCorridor (disallowEntryways: bool) (entryPoint: int) (corridor: char []) : int list =
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

    let possibleMovers (currentState: GameState) : Position list =
        let (rA, rB, rC, rD, c) = currentState

        (posFromRoom rA A)
        @ (posFromRoom rB B)
          @ (posFromRoom rC C)
            @ (posFromRoom rD D) @ (possFromCorridor c)

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
                availableSpaceInCorridor true corridorOut cor2

            availableCorridorPositions
            |> List.map (fun p -> moveToCorridorPos position corridorOut p currentState)
        | Corridor (c, cPos) ->
            let targetRoom =
                if c = 'A' then rA
                else if c = 'B' then rB
                else if c = 'C' then rC
                else rD

            let availableCorridorPositions = availableSpaceInCorridor false cPos cor2

            let targetRoomCorrPos = corridorPosOfRoom (getRoomId c)

            (*printfn
                "%c: target room is at pos %d, available positions are %A"
                c
                targetRoomCorrPos
                availableCorridorPosition*)

            if List.contains targetRoomCorrPos availableCorridorPositions then

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
            else
                []

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

    let isOnly (rID: RoomId) =
        List.forall
            (fun r ->
                match r with
                | Room (id, c, _) -> id = rID && getRoomId c = rID
                | Corridor _ -> false)

    let isEndState (state: GameState) : bool =
        let (rA, rB, rC, rD, cor) = state

        Array.forall ((=) '.') cor
        && isOnly A rA
        && isOnly B rB
        && isOnly C rC
        && isOnly D rD

    let rec move (scores: int list) (acc: int) (state: GameState) : Option<int> =
        //printState state

        if isEndState state then
            Some acc
        else
            let newStates =
                state
                |> possibleMovers
                |> List.map (possibleMoves state)
                |> List.concat
                |> List.sortBy (fun (s, v) -> v)

            if newStates.Length = 0 then
                //printfn "Dead end reached. reverting to previous state"
                None
            else
                let asd =
                    newStates
                    |> List.fold
                        (fun s (newState, cost) ->
                            if acc < s then
                                match move scores (acc + cost) newState with
                                | None -> s
                                | Some v -> min v s
                            else
                                s)
                        1000000

                Some asd

    let part1 (input: string) : string =
        let state = parseInput input
        let (rA, rB, rC, rD, c) = state

        let foo = move [] 0 state

        foo |> Option.get |> string

    let part2 (input: string) : string = input
