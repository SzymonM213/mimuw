package pl.edu.mimuw.matrix.matrices;

import pl.edu.mimuw.matrix.IDoubleMatrix;
import pl.edu.mimuw.matrix.Shape;

import static java.lang.Math.abs;
import static java.lang.Math.sqrt;

public class ColumnMatrix extends EqualLinesMatrix {
    public ColumnMatrix(Shape shape, double[] values) {
        super(shape, values);
    }

    @Override
    public IDoubleMatrix times(double scalar) {
        return new ColumnMatrix(this.shape, super.valuesTimes(scalar));
    }

    @Override
    public IDoubleMatrix plus(IDoubleMatrix other) {
        if (this.getClass() == other.getClass()) {
            assert this.shape.equals(other.shape());
            return new ColumnMatrix(this.shape, this.vectorPlus((VectorMatrix) other));
        } else {
            return super.plus(other);
        }
    }

    @Override
    public IDoubleMatrix plus(double scalar) {
        return new ColumnMatrix(this.shape, this.linePlus(scalar));
    }

    @Override
    public double get(int row, int column) {
        super.getAssert(row, column);
        return this.values[row];
    }

    @Override
    public double[][] data() {
        double[][] twoDimValues = new double[this.shape.rows][this.shape.columns];
        for (int i = 0; i < this.shape.rows; i++) {
            for (int j = 0; j < this.shape.columns; j++) {
                twoDimValues[i][j] = this.values[i];
            }
        }
        return twoDimValues;
    }

    @Override
    public double normOne() {
        double result = 0;
        for (var v : this.values) result += abs(v);
        return result;
    }

    @Override
    public double normInfinity() {
        double result = 0;
        for (var v : this.values) {
            if (abs(v) > result) result = abs(v);
        }
        result *= this.shape.rows;
        return result;
    }

    @Override
    public double frobeniusNorm() {
        double result = 0;
        for (var v : this.values) result += v * v;
        result *= this.shape.columns;
        return sqrt(result);
    }

    @Override
    public String toString() {
        if (this.shape.columns < 3) return super.toString();
        StringBuilder sb = new StringBuilder();
        sb.append("rows: ").append(this.shape.rows).append(", columns: ").append(this.shape.columns).append("\n");
        for (int i = 0; i < this.shape.rows; i++) {
            sb.append(this.values[i]).append(" ... ").append(this.values[i]).append("\n");
        }
        return sb.toString();
    }

    @Override
    public IDoubleMatrix copy() {
        return new ColumnMatrix(this.shape, this.values);
    }
}
