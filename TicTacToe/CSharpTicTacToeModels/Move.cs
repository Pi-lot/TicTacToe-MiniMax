namespace QUT.CSharpTicTacToe {
    public class Move : ITicTacToeMove {
        private readonly int row;
        private readonly int col;

        public Move(int row, int col) {
            this.row = row;
            this.col = col;
        }

        public int Row => GetRow();
        public int Col => GetCol();

        public int GetRow() {
            return row;
        }

        public int GetCol() {
            return col;
        }
    }
}
