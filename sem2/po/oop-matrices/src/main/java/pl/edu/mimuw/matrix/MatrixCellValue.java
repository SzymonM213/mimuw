package pl.edu.mimuw.matrix;

public final class MatrixCellValue {

    public final int row;
    public final double value;
    public final int column;

    public MatrixCellValue(int row, int column, double value) {
        this.column = column;
        this.row = row;
        this.value = value;
    }

    public MatrixCellValue(MatrixCellValue cell) {
        this.column = cell.column;
        this.row = cell.row;
        this.value = cell.value;
    }

    @Override
    public String toString() {
        return "{" + value + " @[" + row + ", " + column + "]}";
    }

    public static MatrixCellValue cell(int row, int column, double value) {
        return new MatrixCellValue(row, column, value);
    }

    public int compareTo(MatrixCellValue other) {
        if ((this.row > other.row) || ((this.row == other.row) && (this.column > other.column))) return 1;
        return -1;
    }
}
