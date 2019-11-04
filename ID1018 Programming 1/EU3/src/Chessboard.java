public class Chessboard {
    private static class Field {
        private ChessPiece piece = null;
        private boolean marked = false;

        public void put(ChessPiece piece) {
            this.piece = piece;
        }

        public ChessPiece take() {
            ChessPiece tempPiece = piece;
            piece = null;

            return tempPiece;
        }

        public void changeMarked() {
            marked = !marked;
        }

        public String toString() {
            String s = (marked) ? " xx" : " --";
            return (piece == null) ? s : piece.toString();
        }
    }

    public static final int NUMBER_OF_ROWS = 8;
    public static final int NUMBER_OF_COLUMNS = 8;
    public static final int FIRST_ROW = 'a';
    public static final int FIRST_COLUMN = 1;
    private Field[][] fields;

    public Chessboard() {
        fields = new Field[NUMBER_OF_ROWS][NUMBER_OF_COLUMNS];
        for (int r = 0; r < NUMBER_OF_ROWS; r++) {
            for (int c = 0; c < NUMBER_OF_COLUMNS; c++) {
                fields[r][c] = new Field();
            }
        }
    }

    public String toString() {
        StringBuilder chessboard = new StringBuilder(" ");
        for (int c = 0; c < NUMBER_OF_COLUMNS; c++) {
            String colRepresentation = " " + (FIRST_COLUMN + c) + " ";
            chessboard.append(colRepresentation);
        }
        chessboard.append("\n");
        for (int r = 0; r < NUMBER_OF_ROWS; r++) {
            chessboard.append((char) (FIRST_ROW + r));

            for (int c = 0; c < NUMBER_OF_COLUMNS; c++) {
                chessboard.append(fields[r][c].toString());
            }
            chessboard.append("\n");
        }

        return chessboard.toString();
    }

    public boolean isValidField(char row, byte column) {
        return row < (FIRST_ROW + 8) && row >= FIRST_ROW && column < (FIRST_COLUMN + 8) && column >= FIRST_COLUMN;
    }

    public abstract class ChessPiece {
        // w - white , b - black
        private char color;
        // K - King , Q - Queen , R - Rook , B - Bishop , N - Knight , P - Pawn
        private char name;
        protected char row = 0;
        protected byte column = -1;

        protected ChessPiece(char color, char name) {
            this.color = color;
            this.name = name;
        }

        public String toString() {
            return " " + color + name;
        }

        public void moveTo(char row, byte column) throws NotValidFieldException {
            if (!Chessboard.this.isValidField(row, column)) {
                throw new NotValidFieldException(" bad field : " + row + column);
            }
            this.row = row;
            this.column = column;
            int r = row - FIRST_ROW;
            int c = column - FIRST_COLUMN;
            Chessboard.this.fields[r][c].put(this);
        }

        public void moveOut() {
            int r = row - FIRST_ROW;
            int c = column - FIRST_COLUMN;
            Chessboard.this.fields[r][c].take();
        }

        public abstract void changeMarkOnReachableFields();
    }

    public class Pawn extends ChessPiece {
        public Pawn(char color, char name) {
            super(color, name);
        }

        public void changeMarkOnReachableFields() {
            byte col = (byte) (column + 1);
            if (Chessboard.this.isValidField(row, col)) {
                int r = row - FIRST_ROW;
                int c = col - FIRST_COLUMN;
                Chessboard.this.fields[r][c].changeMarked();
            }
        }
    }

    public class Rook extends ChessPiece {
        public Rook(char color, char name) {
            super(color, name);
        }

        public void changeMarkOnReachableFields() {
            for (int rowCol = 0; rowCol < 8; rowCol++) {
                //Mark columns
                byte col = (byte) (rowCol + FIRST_COLUMN);
                if (Chessboard.this.isValidField(row, col)) {
                    int r = row - FIRST_ROW;
                    Chessboard.this.fields[r][rowCol].changeMarked();
                }
                //Mark rows
                char row = (char) (rowCol + FIRST_ROW);
                if (Chessboard.this.isValidField(row, column)) {
                    int c = column - FIRST_COLUMN;
                    Chessboard.this.fields[rowCol][c].changeMarked();
                }
            }
        }
    }

    public class Knight extends ChessPiece {
        public Knight(char color, char name) {
            super(color, name);
        }

        public void changeMarkOnReachableFields() {
            int rowNr = row - FIRST_ROW;
            int colNr = column - FIRST_COLUMN;

            for (int i : new int[]{-2, 2}) {
                for (int j : new int[]{-1, 1}) {
                    //Mark above and below
                    char row = (char) (rowNr + i + FIRST_ROW);
                    byte col = (byte) (colNr + j + FIRST_COLUMN);
                    if (Chessboard.this.isValidField(row, col)) {
                        int r = row - FIRST_ROW;
                        int c = col - FIRST_COLUMN;
                        Chessboard.this.fields[r][c].changeMarked();
                    }
                    //Mark left and right
                    row = (char) (rowNr + j + FIRST_ROW);
                    col = (byte) (colNr + i + FIRST_COLUMN);
                    if (Chessboard.this.isValidField(row, col)) {
                        int r = row - FIRST_ROW;
                        int c = col - FIRST_COLUMN;
                        Chessboard.this.fields[r][c].changeMarked();
                    }
                }
            }
        }
    }

    public class Bishop extends ChessPiece {
        public Bishop(char color, char name) {
            super(color, name);
        }

        public void changeMarkOnReachableFields() {
            int rowNr = row - FIRST_ROW;
            int colNr = column - FIRST_COLUMN;

            for (int i = 0; i < 8; i++) {
                for (int j : new int[]{-1, 1}) {
                    //Mark descending diagonal
                    char row = (char) (rowNr + j * i + FIRST_ROW);
                    byte col = (byte) (colNr + j * i + FIRST_COLUMN);
                    if (Chessboard.this.isValidField(row, col)) {
                        int r = row - FIRST_ROW;
                        int c = col - FIRST_COLUMN;
                        Chessboard.this.fields[r][c].changeMarked();
                    }
                    //Mark ascending diagonal
                    col = (byte) (colNr - j * i + FIRST_COLUMN);
                    if (Chessboard.this.isValidField(row, col)) {
                        int r = row - FIRST_ROW;
                        int c = col - FIRST_COLUMN;
                        Chessboard.this.fields[r][c].changeMarked();
                    }
                }
            }
        }
    }

    public class Queen extends ChessPiece {
        public Queen(char color, char name) {
            super(color, name);
        }

        public void changeMarkOnReachableFields() {
            int rowNr = row - FIRST_ROW;
            int colNr = column - FIRST_COLUMN;

            for (int i = 0; i < 8; i++) {
                //Mark horizontal and vertical movement
                byte col = (byte) (i + FIRST_COLUMN);
                if (Chessboard.this.isValidField(row, col)) {
                    int r = row - FIRST_ROW;
                    Chessboard.this.fields[r][i].changeMarked();
                }
                char row = (char) (i + FIRST_ROW);
                if (Chessboard.this.isValidField(row, column)) {
                    int c = column - FIRST_COLUMN;
                    Chessboard.this.fields[i][c].changeMarked();
                }

                //Mark diagonal movement
                for (int j : new int[]{-1, 1}) {
                    row = (char) (rowNr + j * i + FIRST_ROW);
                    col = (byte) (colNr + j * i + FIRST_COLUMN);
                    if (Chessboard.this.isValidField(row, col)) {
                        int r = row - FIRST_ROW;
                        int c = col - FIRST_COLUMN;
                        Chessboard.this.fields[r][c].changeMarked();
                    }
                    col = (byte) (colNr - j * i + FIRST_COLUMN);
                    if (Chessboard.this.isValidField(row, col)) {
                        int r = row - FIRST_ROW;
                        int c = col - FIRST_COLUMN;
                        Chessboard.this.fields[r][c].changeMarked();
                    }
                }
            }
        }
    }

    public class King extends ChessPiece {
        public King(char color, char name) {
            super(color, name);
        }

        public void changeMarkOnReachableFields() {
            int rowNr = row - FIRST_ROW;
            int colNr = column - FIRST_COLUMN;

            for (int i = -1; i <= 1; i++) {
                for (int j = -1; j <= 1; j++) {
                    char row = (char) (rowNr + i + FIRST_ROW);
                    byte col = (byte) (colNr + j + FIRST_COLUMN);
                    if (Chessboard.this.isValidField(row, col)) {
                        int r = row - FIRST_ROW;
                        int c = col - FIRST_COLUMN;
                        Chessboard.this.fields[r][c].changeMarked();
                    }
                }
            }
        }
    }
}