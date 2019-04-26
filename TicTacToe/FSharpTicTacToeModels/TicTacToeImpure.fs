namespace QUT

    module FSharpImpureTicTacToeModel =
    
        type Player = Nought | Cross

        type GameState = 
            {Turn : Player;
            Size : int;
            GetPiece : int * int -> string} 
            interface ITicTacToeGame<Player> with
                member this.Turn with get()    = this.Turn
                member this.Size with get()    = this.Size
                member this.getPiece(row, col) = this.GetPiece (row, col)

        type Move = 
            {Row : int;
             Col : int}
            interface ITicTacToeMove with
                member this.Row with get() = this.Row
                member this.Col with get() = this.Col

        let GameOutcome game     =
            let mutable win = Undecided
            let mutable draws = 0
            for i = 0 to game.Size do
                let mutable numX = 0
                let mutable numO = 0
                let line = seq {for y in 0..(game.Size-1) do
                                    yield (i, y)}
                for j = 0 to game.Size do
                    if game.GetPiece (i, j) = "X" then
                        numX <- numX + 1
                    else if game.GetPiece (i, j) = "O" then
                        numO <- numO + 1
                if numX = game.Size then
                    win <- Win(Cross, line)
                else if numO = game.Size then
                    win <- Win(Nought, line)
                else if numX > 0 && numO > 0 then
                    draws <- draws + 1

            for i = 0 to game.Size do
                let mutable numX = 0
                let mutable numO = 0
                let line = seq {for y in 0..(game.Size-1) do
                                    yield (y, i)}
                for j = 0 to game.Size do
                    if game.GetPiece (j, i) = "X" then
                        numX <- numX + 1
                    else if game.GetPiece (j, i) = "O" then
                        numO <- numO + 1
                if numX = game.Size then
                    win <- Win(Cross, line)
                else if numO = game.Size then
                    win <- Win(Nought, line)

            let mutable numX = 0
            let mutable numO = 0
            
            for i = 0 to game.Size do
                if game.GetPiece (i, i) = "X" then
                    numX <- numX + 1
                else if game.GetPiece (i, i) = "O" then
                    numO <- numO + 1

            let mutable line = seq {for i in 0..game.Size-1 do
                                        yield (i, i)}
            if numX = game.Size then
                win <- Win(Cross, line)
            else if numO = game.Size then
                win <- Win(Nought, line)

            numX <- 0
            numO <- 0

            line <- seq {for i in 0..game.Size-1 do
                            yield (i, game.Size-1 - i)}
            
            for i = 0 to game.Size do
                if game.GetPiece (i, game.Size-1 - i) = "X" then
                    numX <- numX + 1
                else if game.GetPiece (i, game.Size-1 - i) = "O" then
                    numO <- numO + 1

            if numX = game.Size then
                win <- Win(Cross, line)
            else if numO = game.Size then
                win <- Win(Nought, line)
            
            if win = Undecided && draws = game.Size then
                win <- Draw
            else
                win <- win
            win

        let CreateMove row col =
            {Row = row;
             Col = col}

        let ApplyMove game move =
            let newMove = CreateMove move.Row move.Col
            let getPiece (pos: int * int): string = 
                if pos.Equals((newMove.Row, newMove.Col)) then
                    if game.Turn = Nought then "O"
                    else "X"
                else game.GetPiece pos
            let checkTurn turn = 
                if turn = Nought then 
                    Cross
                else
                    Nought
            let newGame = {game with Turn = checkTurn game.Turn; GetPiece = getPiece}
            newGame

        let MoveGenerator game = seq {for i in 0..game.Size-1 do
                                        for j in 0..game.Size-1 do
                                            if game.GetPiece(i, j) = "" then 
                                                yield (i, j)}

        let Heuristic game perspective = 
            let outcome = GameOutcome game
            let mutable num = 0
            if perspective = Cross then
                for i in 0..game.Size do
                    for j in 0..game.Size do
                        if game.GetPiece(i, j) = "X" then
                            num <- num + 1
                        elif game.GetPiece(i, j) = "O" then
                            num <- num - 1
                match outcome with
                | Win(Cross, _) -> 27
                | Draw -> 0
                | Win(Nought, _) -> -27
                | Undecided -> num
            else
                for i in 0..game.Size do
                    for j in 0..game.Size do
                        if game.GetPiece(i, j) = "O" then
                            num <- num + 1
                        elif game.GetPiece(i, j) = "X" then
                            num <- num - 1
                match outcome with
                | Win(Nought, _) -> 27
                | Draw -> 0
                | Win(Cross, _) -> -27
                | Undecided -> num

        let getTurn game = game.Turn

        let GameOver game =
            if GameOutcome game = Undecided then
                false
            else
                true

        let applyMove (oldState:GameState) (move: int*int) =
            let row = 
                match move with
                | (a, _) -> a
            let col = 
                match move with
                | (_, b) -> b
            let newMove = CreateMove row col
            let getPiece (pos: int * int): string = 
                if pos.Equals((newMove.Row, newMove.Col)) then
                    if oldState.Turn = Nought then "O"
                    else "X"
                else oldState.GetPiece pos
            let checkTurn turn = 
                if turn = Nought then 
                    Cross
                else
                    Nought
            let newState = {oldState with Turn = checkTurn oldState.Turn; GetPiece = getPiece}
            newState 

        let FindBestMove game = 
            NodeCounter.Reset()
            let MiniMax = GameTheory.MiniMaxWithAlphaBetaPruningGenerator Heuristic getTurn GameOver MoveGenerator applyMove
            let best = MiniMax System.Int32.MinValue System.Int32.MaxValue game game.Turn
            let move = 
                match best with
                | (Some move, _) -> move
            let row =
                match move with
                | (row, _) -> row
            let col =
                match move with
                | (_, col) -> col
            {Row = row;
             Col = col}

        let GameStart first size =
            let getPiece pos = ""
            {Turn = first;
             Size = size;
             GetPiece = getPiece}

        type WithAlphaBetaPruning() =
            override this.ToString()         = "Impure F# with Alpha Beta Pruning";
            interface ITicTacToeModel<GameState, Move, Player> with
                member this.Cross with get()             = Cross
                member this.Nought with get()            = Nought
                member this.GameStart(firstPlayer, size) = GameStart firstPlayer size
                member this.CreateMove(row, col)         = CreateMove row col
                member this.GameOutcome(game)            = GameOutcome game 
                member this.ApplyMove(game, move)        = ApplyMove game  move
                member this.FindBestMove(game)           = FindBestMove game