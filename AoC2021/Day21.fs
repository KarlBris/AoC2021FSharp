namespace AoC2021

open Utils

module Day21 =
    type UniverseKey = (int * ((int * int) * (int * int)))

    type DiracMap = Map<UniverseKey, int64>

    let rollDie (last: int) : int =
        match last with
        | 100 -> 1
        | _ -> last + 1

    let rec nextNThrows (last: int) (n: int) : int =
        match n with
        | 1 -> rollDie last
        | _ ->
            let roll = rollDie last
            roll + (nextNThrows roll (n - 1))

    let takeSteps (pos: int) (steps: int) : int = ((pos + steps - 1) % 10) + 1

    let rec playGame
        (p1Pos: int)
        (p2Pos: int)
        (p1Score: int)
        (p2Score: int)
        (lastDiceThrow: int)
        (diceThrownCount: int)
        (playerTurn: int)
        (goalScore: int)
        : (int * int) * int =

        if p1Score >= goalScore || p2Score >= goalScore then
            ((p1Score, p2Score), diceThrownCount)
        else
            let nextThreeRolls = nextNThrows lastDiceThrow 3

            if playerTurn = 1 then
                let p1Pos2 = takeSteps p1Pos nextThreeRolls

                playGame
                    p1Pos2
                    p2Pos
                    (p1Score + p1Pos2)
                    p2Score
                    (((lastDiceThrow + 3 - 1) % 100) + 1)
                    (diceThrownCount + 3)
                    (2)
                    goalScore
            else
                let p2Pos2 = takeSteps p2Pos nextThreeRolls

                playGame
                    p1Pos
                    p2Pos2
                    p1Score
                    (p2Score + p2Pos2)
                    (((lastDiceThrow + 3 - 1) % 100) + 1)
                    (diceThrownCount + 3)
                    (1)
                    goalScore

    let part1 (input: string) : string =
        let [| p1String; p2String |] = input |> lines
        let p1Start = p1String.[28..] |> int
        let p2Start = p2String.[28..] |> int

        let ((p1Score, p2Score), thrown) = playGame p1Start p2Start 0 0 0 0 1 1000

        (min p1Score p2Score) * thrown |> string

    let newPositions (pos: int) =
        Array.allPairs [| 1; 2; 3 |] [|
            1
            2
            3
        |]
        |> Array.allPairs [| 1; 2; 3 |]
        |> Array.map (fun (a, (b, c)) -> a + b + c)
        |> Array.map (fun steps -> takeSteps pos steps)

    let playQuantum (input: UniverseKey * int64) : (UniverseKey * int64) [] =
        let ((turn, ((p1pos, p1sc), (p2pos, p2sc))), num) = input

        if p1sc >= 21 || p2sc >= 21 then
            [| input |]
        else if turn = 1 then
            newPositions p1pos
            |> Array.map (fun newPos -> ((2, ((newPos, p1sc + newPos), (p2pos, p2sc))), num))
        else
            newPositions p2pos
            |> Array.map (fun newPos -> ((1, ((p1pos, p1sc), (newPos, p2sc + newPos))), num))

    let optionAdd (op: Option<int64>) (v: int64) : Option<int64> =
        match op with
        | Some value -> Some(value + v)
        | None -> Some v

    let addArrayToMap (input: (UniverseKey * int64) []) : DiracMap =
        input
        |> Array.fold (fun state (k, v) -> Map.change k (fun op -> optionAdd op v) state) Map.empty

    let rec playCoolGame (input: DiracMap) : int64 =
        let newMap =
            Map.toArray input
            |> Array.map playQuantum
            |> Array.concat
            |> addArrayToMap

        if Map.count newMap = Map.count input then
            let (p1WinOutcomes, p2WinOutcomes) =
                newMap
                |> Map.toArray
                |> Array.partition (fun ((turn, _), _) -> turn = 2)

            let p1Score =
                p1WinOutcomes |> Array.map snd |> Array.sum

            let p2Score =
                p2WinOutcomes |> Array.map snd |> Array.sum

            max p1Score p2Score
        else
            playCoolGame newMap

    let part2 (input: string) : string =
        let [| p1String; p2String |] = input |> lines
        let p1Start = p1String.[28..] |> int
        let p2Start = p2String.[28..] |> int

        playCoolGame (Map.ofArray [| ((1, ((p1Start, 0), (p2Start, 0))), 1L) |])
        |> string
