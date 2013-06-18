package chess.movelogic.bitboard;


public class BitOps {


    public static long unsignedShiftRight(long bb, int n) {
        return bb >>> n;

    }

}
