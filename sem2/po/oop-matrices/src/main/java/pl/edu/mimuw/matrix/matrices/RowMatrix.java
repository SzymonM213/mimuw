package pl.edu.mimuw.matrix.matrices;

import pl.edu.mimuw.matrix.IDoubleMatrix;
import pl.edu.mimuw.matrix.Shape;

import static java.lang.Math.abs;
import static java.lang.Math.sqrt;

public class RowMatrix extends EqualLinesMatrix {
    public RowMatrix(Shape shape, double[] values) {
        super(shape, values);
    }

    @Override
    public IDoubleMatrix times(double scalar) {
        return new RowMatrix(this.shape, super.valuesTimes(scalar));
    }


    @Override
    public IDoubleMatrix plus(IDoubleMatrix other) {
        if (this.getClass() == other.getClass()) {
            assert this.shape.equals(other.shape());
            return new RowMatrix(this.shape, this.vectorPlus((VectorMatrix) other));
        } else {
            return super.plus(other);
        }
    }

    @Override
    public IDoubleMatrix plus(double scalar) {
        return new RowMatrix(this.shape, this.linePlus(scalar));
    }

    @Override
    public double get(int row, int column) {
        super.getAssert(row, column);
        return this.values[column];
    }

    @Override
    public double[][] data() {
        double[][] twoDimValues = new double[this.shape.rows][this.shape.columns];
        for (int i = 0; i < this.shape.rows; i++) {
            for (int j = 0; j < this.shape.columns; j++) {
                twoDimValues[i][j] = this.values[j];
            }
        }
        return twoDimValues;
    }

    @Override
    public double normOne() {
        double result = 0;
        for (var v : this.values) {
            if (abs(v) > result) result = abs(v);
        }
        result *= this.shape.rows;
        return result;
    }

    @Override
    public double normInfinity() {
        double result = 0;
        for (var v : this.values) result += abs(v);
        return result;
    }

    @Override
    public double frobeniusNorm() {
        double result = 0;
        for (var v : this.values) result += v * v;
        result *= this.shape.rows;
        return sqrt(result);
    }

    @Override
    public IDoubleMatrix copy() {
        return new RowMatrix(this.shape, this.values);
    }
}
