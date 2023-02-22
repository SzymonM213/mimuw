package pl.edu.mimuw.matrix.matrices;

import pl.edu.mimuw.matrix.IDoubleMatrix;
import pl.edu.mimuw.matrix.Shape;

public class NormalDiagonalMatrix extends DiagonalMatrix {

    public NormalDiagonalMatrix(Shape shape, double[] values) {
        super(shape, values);
    }

    @Override
    public IDoubleMatrix times(double scalar) {
        return new NormalDiagonalMatrix(this.shape, super.valuesTimes(scalar));
    }

    @Override
    public IDoubleMatrix plus(IDoubleMatrix other) {
        if (this.getClass() == other.getClass()) {
            return new NormalDiagonalMatrix(this.shape, this.vectorPlus((VectorMatrix) other));
        } else {
            return super.plus(other);
        }
    }

    @Override
    public double get(int row, int column) {
        super.getAssert(row, column);
        if (row == column) return this.values[row];
        else return 0;
    }

    @Override
    public double[][] data() {
        double[][] twoDimValues = new double[this.shape.rows][this.shape.columns];
        for (int i = 0; i < this.shape.rows; i++) {
            for (int j = 0; j < this.shape.columns; j++) {
                if (i == j) twoDimValues[i][j] = values[i];
                else twoDimValues[i][j] = 0;
            }
        }
        return twoDimValues;
    }

    @Override
    public String toString() {
        if (this.shape.columns < 4) return super.toString();
        StringBuilder sb = new StringBuilder();
        sb.append("rows: ").append(this.shape.rows).append(", columns: ").append(this.shape.columns).append("\n");
        for (int i = 0; i < this.shape.columns; i++) {
            for (int j = 0; j < i; j++) {
                sb.append(this.get(i, j)).append(" ");
                if (j == 0 && i > 2) {
                    sb.append("... ");
                    j = i - 2;
                }
            }
            sb.append(this.get(i, i)).append(" ");
            for (int j = i + 1; j < this.shape.columns; j++) {
                sb.append(this.get(i, j)).append(" ");
                if (j == i + 1 && i < this.shape.columns - 3) {
                    sb.append("... ");
                    j = this.shape.columns - 2;
                }
            }
            sb.append("\n");
        }
        return sb.toString();
    }

    @Override
    public IDoubleMatrix copy() {
        return new NormalDiagonalMatrix(this.shape, this.values);
    }
}
