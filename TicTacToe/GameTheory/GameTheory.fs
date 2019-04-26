namespace QUT

    module GameTheory =
        open System

        let MiniMaxGenerator (heuristic:'Game -> 'Player -> int) (getTurn: 'Game -> 'Player) (gameOver: 'Game -> bool) (moveGenerator: 'Game -> seq<'Move>) (applyMove: 'Game -> 'Move -> 'Game) : 'Game -> 'Player -> Option<'Move> * int =
            // Basic MiniMax algorithm without using alpha beta pruning
            let rec MiniMax game perspective =
                NodeCounter.Increment()
                if gameOver game = true then 
                    (None, (heuristic game perspective))
                else
                    let moves = moveGenerator game
                    let movesL = Seq.toList(moves)
                    let rec findBest (best: Option<'Move> * int) (pMoves: list<'Move>) =
                        if getTurn game = perspective then
                            match pMoves with
                            | [] -> best
                            | m::pMoves' -> let maxV =
                                                match best with
                                                | (Some _, maxV) -> maxV
                                                | (None, _) -> System.Int32.MinValue
                                            let recur = MiniMax (applyMove game m) perspective
                                            let cVal =
                                                match recur with
                                                | (Some _, v) -> v
                                                | (None, _) -> heuristic (applyMove game m) perspective
                                            if cVal > maxV then
                                                findBest ((Some m), cVal) pMoves'
                                            else
                                                findBest best pMoves'
                        else
                            match pMoves with
                            | [] -> best
                            | m::pMoves' -> let minV =
                                                match best with
                                                | (Some _, minV) -> minV
                                                | (None, _) -> System.Int32.MaxValue
                                            let recur = MiniMax (applyMove game m) perspective
                                            let cVal =
                                                match recur with
                                                | (Some _, v) -> v
                                                | (None, _) -> heuristic (applyMove game m) perspective
                                            if cVal < minV then
                                                findBest ((Some m), cVal) pMoves'
                                            else
                                                findBest best pMoves'
                    findBest (None, 0) movesL
            NodeCounter.Reset()
            MiniMax

        let MiniMaxWithAlphaBetaPruningGenerator (heuristic:'Game -> 'Player -> int) (getTurn: 'Game -> 'Player) (gameOver: 'Game -> bool) (moveGenerator: 'Game -> seq<'Move>) (applyMove: 'Game -> 'Move -> 'Game) : int -> int -> 'Game -> 'Player -> Option<'Move> * int =
            // Optimized MiniMax algorithm that uses alpha beta pruning to eliminate parts of the search tree that don't need to be explored            
            let rec MiniMax alpha beta oldState perspective =
                NodeCounter.Increment()
                if gameOver oldState = true then 
                    (None, heuristic oldState perspective)
                else
                    let moves = moveGenerator oldState
                    let movesL = Seq.toList(moves)
                    let rec findBest (best: Option<'Move> * int) (pMoves: list<'Move>) =
                        if getTurn oldState = perspective then
                            match pMoves with
                            | [] -> best
                            | m::pMoves' -> let maxV =
                                                match best with
                                                | (Some _, maxV) -> maxV
                                                | (None, _) -> System.Int32.MinValue
                                            let newState = applyMove oldState m
                                            let recur = MiniMax alpha maxV newState perspective
                                            let cVal =
                                                match recur with
                                                | (Some _, v) -> v
                                                | (None, _) -> heuristic newState perspective
                                            if cVal >= alpha && beta <= alpha then
                                                ((Some m), cVal)
                                            elif cVal > maxV then
                                                findBest ((Some m), cVal) pMoves'
                                            else
                                                findBest best pMoves'
                        else
                            match pMoves with
                            | [] -> best
                            | m::pMoves' -> let minV =
                                                match best with
                                                | (Some _, minV) -> minV
                                                | (None, _) -> System.Int32.MaxValue
                                            let newState = applyMove oldState m
                                            let recur = MiniMax minV beta newState perspective
                                            let cVal =
                                                match recur with
                                                | (Some _, v) -> v
                                                | (None, _) -> heuristic newState perspective
                                            if cVal <= beta && beta <= alpha then
                                                ((Some m), cVal)
                                            elif cVal < minV then
                                                findBest ((Some m), cVal) pMoves'
                                            else
                                                findBest best pMoves'
                    findBest (None, 0) movesL
            NodeCounter.Reset()
            MiniMax
