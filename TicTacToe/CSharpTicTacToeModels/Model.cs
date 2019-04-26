namespace QUT.CSharpTicTacToe {
    using System.Collections.Generic;
    using QUT;
    using System;
    using Microsoft.FSharp.Core;

    public class WithAlphaBetaPruning : ITicTacToeModel<Game, Move, Player> {
        public Player Cross => Player.Cross;
        public Player Nought => Player.Nought;

        public override string ToString() {
            return "Impure C# with Alpha Beta Pruning";
        }

        public Game ApplyMove(Game game, Move move) {
            Game game1 = new Game(game, move);
            return game1;
        }

        public Move CreateMove(int row, int col) {
            Move m = new Move(row, col);
            return m;
        }

        public Player GetTurn(Game game) {
            return game.Turn;
        }

        public IEnumerable<Move> MoveGenerator(Game game) {
            List<Move> line = new List<Move>();

            Move move;

            for (int i = 0; i < game.Size; i++) {
                for (int j = 0; j < game.Size; j++) {
                    if (game.getPiece(i, j) == "") {
                        move = new Move(i, j);
                        line.Add(move);
                    }
                }
            }

            return line;
        }

        public int Heuristic(Game game, Player perspective) {
            if (GameOutcome(game) == TicTacToeOutcome<Player>.Draw)
                return 0;
            else if (GameOutcome(game).IsWin) {
                var win = GameOutcome(game) as TicTacToeOutcome<Player>.Win;
                if (win.winner == perspective)
                    return 27;
                else
                    return -27;
            } else {
                string player;
                if (perspective == Player.Cross)
                    player = "X";
                else
                    player = "O";

                int score = 0;

                for (int i = 0; i < game.Size; i++) {
                    for (int j = 0; j < game.Size; j++) {
                        if (game.getPiece(i, j) == player)
                            score++;
                        else if (game.getPiece(i, j) != player && game.getPiece(i, j) != "")
                            score--;
                    }
                }

                return score;
            }
        }

        public bool GameOver(Game game) {
            if (GameOutcome(game) != TicTacToeOutcome<Player>.Undecided)
                return true;
            else
                return false;
        }

        public Move FindBestMove(Game game) {
            FSharpFunc<Game, bool> gameOver = FuncConvert.FromFunc<Game, bool>(GameOver);
            FSharpFunc<Game, FSharpFunc<Player, int>> heuristic = FuncConvert.FromFunc<Game, Player, int>(Heuristic);
            FSharpFunc<Game, Player> getTurn = FuncConvert.FromFunc<Game, Player>(GetTurn);
            FSharpFunc<Game, IEnumerable<Move>> moveGenerator = FuncConvert.FromFunc<Game, IEnumerable<Move>>(MoveGenerator);
            FSharpFunc<Game, FSharpFunc<Move, Game>> applyMove = FuncConvert.FromFunc<Game, Move, Game>(ApplyMove);
            OptimizedClosures.FSharpFunc<int, int, Game, Player, Tuple<FSharpOption<Move>, int>> mini =
                (OptimizedClosures.FSharpFunc<int, int, Game, Player, Tuple<FSharpOption<Move>, int>>)GameTheory.MiniMaxWithAlphaBetaPruningGenerator(heuristic,
                getTurn, gameOver, moveGenerator, applyMove);

            NodeCounter.Reset();
            Tuple<FSharpOption<Move>, int> best = mini.Invoke(int.MinValue, int.MaxValue, game, game.Turn);
            Move move = best.Item1.Value;
            return move;
        }


        public TicTacToeOutcome<Player> GameOutcome(Game game) {
            Player winner;

            bool win = false;
            bool draw = false;

            int winningLine = 0;

            string nought = "O";
            string cross = "X";

            int draws = 0;

            int rowCountX = 0;
            int rowCountO = 0;

            int colCountX = 0;
            int colCountO = 0;

            int dia0CountX = 0;
            int dia0CountO = 0;
            int dia1CountX = 0;
            int dia1CountO = 0;

            for (int i = 0; i < game.Size; i++) {
                for (int j = 0; j < game.Size; j++) {
                    if (game.getPiece(i, j) == cross)
                        rowCountX++;
                    else if (game.getPiece(i, j) == nought)
                        rowCountO++;

                    if (game.getPiece(j, i) == cross)
                        colCountX++;
                    else if (game.getPiece(j, i) == nought)
                        colCountO++;
                }

                if (game.getPiece(i, i) == cross)
                    dia0CountX++;
                else if (game.getPiece(i, i) == nought)
                    dia0CountO++;

                if (game.getPiece(game.Size - 1 - i, i) == cross)
                    dia1CountX++;
                else if (game.getPiece(game.Size - 1 - i, i) == nought)
                    dia1CountO++;

                if (rowCountO == game.Size || rowCountX == game.Size || colCountO == game.Size || colCountX == game.Size ||
                    dia0CountO == game.Size || dia0CountX == game.Size || dia1CountO == game.Size || dia1CountX == game.Size)
                    win = true;

                else if (rowCountO + rowCountX == game.Size)
                    draws++;

                winningLine = i;

                if (win)
                    break;

                rowCountO = 0;
                rowCountX = 0;
                colCountO = 0;
                colCountX = 0;
            }

            List<System.Tuple<int, int>> line = new List<System.Tuple<int, int>>();

            System.Tuple<int, int> tuple;
            if (rowCountO == game.Size || rowCountX == game.Size) {
                for (int i = 0; i < game.Size; i++) {
                    tuple = new System.Tuple<int, int>(winningLine, i);
                    line.Add(tuple);
                }
            } else if (colCountO == game.Size || colCountX == game.Size) {
                for (int i = 0; i < game.Size; i++) {
                    tuple = new System.Tuple<int, int>(i, winningLine);
                    line.Add(tuple);
                }
            } else if (dia0CountO == game.Size || dia0CountX == game.Size) {
                for (int i = 0; i < game.Size; i++) {
                    tuple = new System.Tuple<int, int>(i, i);
                    line.Add(tuple);
                }
            } else {
                for (int i = 0; i < game.Size; i++) {
                    tuple = new System.Tuple<int, int>(game.Size - 1 - i, i);
                    line.Add(tuple);
                }
            }


            if (draws == game.Size)
                draw = true;

            if (win) {
                if (rowCountO == game.Size || colCountO == game.Size || dia0CountO == game.Size || dia1CountO == game.Size)
                    winner = Player.Nought;
                else
                    winner = Player.Cross;
                return TicTacToeOutcome<Player>.NewWin(winner, line);
            } else if (draw)
                return TicTacToeOutcome<Player>.Draw;
            else
                return TicTacToeOutcome<Player>.Undecided;
        }

        public Game GameStart(Player first, int size) {
            Game game = new Game(first, size);
            return game;
        }
    }
}