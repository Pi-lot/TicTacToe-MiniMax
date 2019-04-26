namespace QUT

    module FSharpPureTicTacToeModel =
        // type to represent the two players: Noughts and Crosses
        type Player = Nought | Cross

        // type to represent a single move specified using (row, column) coordinates of the selected square
        type Move = 
            {Row : int;
             Col : int}
            interface ITicTacToeMove with
                member this.Row with get() = this.Row
                member this.Col with get() = this.Col

        // type to represent the current state of the game, including the size of the game (NxN), who's turn it is and the pieces on the board
        type GameState = 
            {Turn : Player;
             Size : int;
             GetPiece : int * int -> string}
            interface ITicTacToeGame<Player> with
                member this.Turn with get()    = this.Turn
                member this.Size with get()    = this.Size
                member this.getPiece(row, col) = this.GetPiece (row, col)

        let CreateMove row col =
            {Row = row;
             Col = col}

        let ApplyMove (oldState:GameState) (move: Move) =
            let newMove = CreateMove move.Row move.Col
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

        // Returns a sequence containing all of the lines on the board: Horizontal, Vertical and Diagonal
        // The number of lines returned should always be (size*2+2)
        // the number of squares in each line (represented by (row,column) coordinates) should always be equal to size
        // For example, if the input size = 2, then the output would be: 
        //     seq [seq[(0,0);(0,1)];seq[(1,0);(1,1)];seq[(0,0);(1,0)];seq[(0,1);(1,1)];seq[(0,0);(1,1)];seq[(0,1);(1,0)]]
        // The order of the lines and the order of the squares within each line does not matter
        let Lines (size:int) : seq<seq<int*int>> = seq {for j in 0..(size-1) do
                                                            yield! seq[seq {for x in 0..(size-1) do
                                                                                yield (j, x)}]}

        // Checks a single line (specified as a sequence of (row,column) coordinates) to determine if one of the players
        // has won by filling all of those squares, or a Draw if the line contains at least one Nought and one Cross
        let CheckLine (game:GameState) (line:seq<int*int>) : TicTacToeOutcome<Player> = 
            let numX pred = Seq.filter pred >> Seq.length
            let countX = line |> numX (fun n -> game.GetPiece n = "X")
            let countO = line |> numX (fun n -> game.GetPiece n = "O")

            if countX = game.Size then
                Win(Cross, line)
            else if countO = game.Size then
                Win(Nought, line)
            else if countO + countX = game.Size then
                Draw
            else
                Undecided

        let GameOutcome game =
            let lines = Lines game.Size
            let line pred = Seq.filter pred >> Seq.length

            let draws = lines |> line (fun n -> CheckLine game n = Draw)
            let wins = lines |> line (fun n -> not ((CheckLine game n = Undecided) || (CheckLine game n = Draw)))

            let outcomes = Seq.toList(lines)
            let checkWin line = not ((CheckLine game line = Undecided) || (CheckLine game line = Draw))

            let columns = seq {for j in 0..(game.Size-1) do
                                   yield! seq[seq {for x in 0..(game.Size-1) do
                                                       yield (x, j)}]}
            
            let colWins = columns |> line (fun n -> not ((CheckLine game n = Undecided) || (CheckLine game n = Draw)))
            let colOutcomes = Seq.toList(columns)

            let diagonal0 = seq {for i in 0..game.Size-1 do
                                    yield (i, i)}

            let diagonal1 = seq {for i in 0..game.Size-1 do
                                    yield (i, game.Size-1 - i)}
            
            if wins > 0 then
                let win = List.find(checkWin) outcomes
                CheckLine game win
            else if colWins > 0 then
                let win = List.find(checkWin) colOutcomes
                CheckLine game win
            else if not ((CheckLine game diagonal0 = Undecided) || (CheckLine game diagonal0 = Draw)) then
                CheckLine game diagonal0
            else if not ((CheckLine game diagonal1 = Undecided) || (CheckLine game diagonal1 = Draw)) then
                CheckLine game diagonal1
            else if draws = game.Size then
                Draw
            else
                Undecided

        let GameStart (firstPlayer:Player) size = 
            let getPiece pos = ""
            { Turn = firstPlayer;
              Size = size;
              GetPiece = getPiece}

        let MoveGenerator game = seq {for i in 0..game.Size-1 do
                                          for j in 0..game.Size-1 do
                                              if game.GetPiece(i, j) = "" then 
                                                  yield (i, j)}

        let Heuristic game perspective = 
            let outcome = GameOutcome game
            if perspective = Cross then
                match outcome with
                | Win(Cross, _) -> 27
                | Draw -> 0
                | Win(Nought, _) -> -27
                | Undecided -> let rec count value index =
                                   let rec c v i =
                                       if i > game.Size && index > game.Size then
                                           value
                                       elif i > game.Size then
                                           count v (index + 1)
                                       else 
                                           if game.GetPiece(index, i) = "O" then
                                               c (v + 1) (i + 1)
                                           elif game.GetPiece(index, i) = "X" then
                                               c (v - 1) (i + 1)
                                           else
                                               c v (i + 1)
                                   c value 0
                               count 0 0
            else
                match outcome with
                | Win(Nought, _) -> 27
                | Draw -> 0
                | Win(Cross, _) -> -27
                | Undecided -> let rec count value index =
                                   let rec c v i =
                                       if i > game.Size && index > game.Size then
                                           value
                                       elif i > game.Size then
                                           count v (index + 1)
                                       else 
                                           if game.GetPiece(index, i) = "O" then
                                               c (v + 1) (i + 1)
                                           elif game.GetPiece(index, i) = "X" then
                                               c (v - 1) (i + 1)
                                           else
                                               c v (i + 1)
                                   c value 0
                               count 0 0

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

        let MiniMax game = 
            let Mini = GameTheory.MiniMaxGenerator Heuristic getTurn GameOver MoveGenerator applyMove
            NodeCounter.Reset()
            Mini game

        let MiniMaxWithPruning game = 
            let Mini = GameTheory.MiniMaxWithAlphaBetaPruningGenerator Heuristic getTurn GameOver MoveGenerator applyMove
            NodeCounter.Reset()
            Mini System.Int32.MinValue System.Int32.MaxValue game

        let FindBestMoveBasic game =
            NodeCounter.Reset()
            let best = MiniMax game game.Turn
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

        let FindBestMove game =
            NodeCounter.Reset()
            let best = MiniMaxWithPruning game game.Turn
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

        [<AbstractClass>]
        type Model() =
            abstract member FindBestMove : GameState -> Move
            interface ITicTacToeModel<GameState, Move, Player> with
                member this.Cross with get()             = Cross 
                member this.Nought with get()            = Nought 
                member this.GameStart(firstPlayer, size) = GameStart firstPlayer size
                member this.CreateMove(row, col)         = CreateMove row col
                member this.GameOutcome(game)            = GameOutcome game
                member this.ApplyMove(game, move)        = ApplyMove game move 
                member this.FindBestMove(game)           = this.FindBestMove game

        type BasicMiniMax() =
            inherit Model()
            override this.ToString()         = "Pure F# with basic MiniMax";
            override this.FindBestMove(game) = FindBestMoveBasic game


        type WithAlphaBetaPruning() =
            inherit Model()
            override this.ToString()         = "Pure F# with Alpha Beta Pruning";
            override this.FindBestMove(game) = FindBestMove game