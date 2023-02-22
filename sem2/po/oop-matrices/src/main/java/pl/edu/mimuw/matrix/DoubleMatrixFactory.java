package pl.edu.mimuw.matrix;

import pl.edu.mimuw.matrix.matrices.*;

import static pl.edu.mimuw.matrix.Shape.matrix;

public class DoubleMatrixFactory {

    private DoubleMatrixFactory() {
    }

    public static IDoubleMatrix sparse(Shape shape, MatrixCellValue... values) {
        assert values != null;
        assert shape != null;
        return new SparseMatrix(shape, values);
    }

    public static IDoubleMatrix full(double[][] values) {
        assert values != null;
        assert values.length != 0;
        return new FullMatrix(matrix(values.length, values[0].length), values);
    }

    public static IDoubleMatrix identity(int size) {
        return new IdentityMatrix(matrix(size, size));
    }

    public static IDoubleMatrix diagonal(double... diagonalValues) {
        assert diagonalValues != null;
        return new NormalDiagonalMatrix(matrix(diagonalValues.length, diagonalValues.length), diagonalValues);
    }

    public static IDoubleMatrix antiDiagonal(double... antiDiagonalValues) {
        assert antiDiagonalValues != null;
        return new AntiDiagonalMatrix(matrix(antiDiagonalValues.length, antiDiagonalValues.length), antiDiagonalValues);
    }

    public static IDoubleMatrix vector(double... values) {
        assert values != null && values.length != 0;
        return new JustVector(Shape.vector(values.length), values);
    }

    public static IDoubleMatrix zero(Shape shape) {
        assert shape != null;
        return new ConstantMatrix(shape, 0);
    }
    
    public static IDoubleMatrix column(Shape shape, double[] columnValues) {
        assert columnValues != null;
        assert shape != null;
        assert shape.rows == columnValues.length;
        return new ColumnMatrix(shape, columnValues);
    }

    public static IDoubleMatrix row(Shape shape, double[] rowValues) {
        assert rowValues != null;
        assert shape != null;
        assert shape.columns == rowValues.length;
        return new RowMatrix(shape, rowValues);
    }

    public static IDoubleMatrix constant(Shape shape, double value) {
        assert shape != null;
        return new ConstantMatrix(shape, value);
    }
}
