using System.Collections.Generic;
using System;

namespace QUT.CSharpTicTacToe {
    public class Game : ITicTacToeGame<Player> {
        private string[][] pieces; // Use this to keep track of the pieces on the board
        public int Size { get; }
        public Player Turn { get; }
        public Game(Player turn, int size) {
            Turn = turn;
            Size = size;

            pieces = new string[size][];

            for (int i = 0; i < size; i++) {
                pieces[i] = new string[size];
                for (int j = 0; j < size; j++)
                    pieces[i][j] = "";
            }
        }

        public Game(Game game, Move move) {
            if (game.Turn == Player.Cross)
                Turn = Player.Nought;
            else
                Turn = Player.Cross;

            Size = game.Size;

            pieces = new string[Size][];

            for (int i = 0; i < Size; i++) {
                pieces[i] = new string[Size];
                for (int j = 0; j < Size; j++) {
                    if (i == move.Row && j == move.Col) {
                        if (game.Turn == Player.Cross)
                            pieces[i][j] = "X";
                        else
                            pieces[i][j] = "O";
                    } else
                        pieces[i][j] = game.getPiece(i, j);
                }
            }
        }

        public string getPiece(int row, int col) {
            return pieces[row][col];
        }
    }
}