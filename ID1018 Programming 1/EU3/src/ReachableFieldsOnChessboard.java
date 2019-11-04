import java.util.Random;

public class ReachableFieldsOnChessboard {
    public static void main(String[] args) {
        Chessboard chessBoard = new Chessboard ();
        System.out.println (chessBoard + "\n");
        Chessboard.ChessPiece[] pieces = new Chessboard.ChessPiece[6];
        pieces[0] = chessBoard.new Pawn ('w', 'P');
        pieces[1] = chessBoard.new Rook ('b', 'R');
        pieces[2] = chessBoard.new Queen ('w', 'Q');
        pieces[3] = chessBoard.new Bishop ('w', 'B');
        pieces[4] = chessBoard.new King ('b', 'K');
        pieces[5] = chessBoard.new Knight ('w', 'N');

        Random rand = new Random();
        for (Chessboard.ChessPiece piece : pieces) {
            char row = (char) (Chessboard.FIRST_ROW + rand.nextInt(Chessboard.NUMBER_OF_ROWS));
            byte col = (byte) (Chessboard.FIRST_COLUMN + rand.nextInt(8));

            try {
                piece.moveTo(row, col);
                piece.changeMarkOnReachableFields();

                System.out.println(chessBoard.toString());

                piece.changeMarkOnReachableFields();
                piece.moveOut();
            } catch (NotValidFieldException ex) {
                System.out.println(ex.getMessage());
            }
        }
    }
}
