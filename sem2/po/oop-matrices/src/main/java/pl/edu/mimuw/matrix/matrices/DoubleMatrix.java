package pl.edu.mimuw.matrix.matrices;

import pl.edu.mimuw.matrix.IDoubleMatrix;
import pl.edu.mimuw.matrix.Shape;

import static pl.edu.mimuw.matrix.Shape.matrix;

import static java.lang.Math.abs;
import static java.lang.Math.sqrt;

public abstract class DoubleMatrix implements IDoubleMatrix {

    protected final Shape shape;

    public DoubleMatrix(Shape shape) {
        this.shape = matrix(shape.rows, shape.columns);
    }

    @Override
    public IDoubleMatrix times(IDoubleMatrix other) {
        assert this.shape.columns == other.shape().rows;
        double[][] productValues = new double[this.shape.rows][other.shape().columns];
        for (int row = 0; row < this.shape.rows; row++) {
            for (int column = 0; column < other.shape().columns; column++) {
                productValues[row][column] = 0;
                for (int i = 0; i < this.shape.columns; i++) {
                    productValues[row][column] += this.get(row, this.shape.columns - 1 - i) *
                            other.get(other.shape().rows - 1 - i, column);
                }
            }
        }
        return new FullMatrix(matrix(this.shape.rows, other.shape().columns), productValues);
    }

    @Override
    public IDoubleMatrix times(double scalar) {
        double[][] productValues = new double[this.shape.rows][this.shape.columns];
        for (int row = 0; row < this.shape.rows; row++) {
            for (int column = 0; column < this.shape.columns; column++) {
                productValues[row][column] = this.get(row, column) * scalar;
            }
        }
        return new FullMatrix(matrix(this.shape.rows, this.shape.columns), productValues);
    }

    @Override
    public IDoubleMatrix plus(IDoubleMatrix other) {
        assert this.shape.equals(other.shape());
        double[][] sumValues = new double[this.shape.rows][this.shape.columns];
        for (int i = 0; i < this.shape.rows; i++) {
            for (int j = 0; j < this.shape.columns; j++) {
                sumValues[i][j] = this.get(i, j) + other.get(i, j);
            }
        }
        return new FullMatrix(this.shape, sumValues);
    }

    @Override
    public IDoubleMatrix plus(double scalar) {
        double[][] sumValues = new double[this.shape.rows][this.shape.columns];
        for (int i = 0; i < this.shape.rows; i++) {
            for (int j = 0; j < this.shape.columns; j++) {
                sumValues[i][j] = this.get(i, j) + scalar;
            }
        }
        return new FullMatrix(this.shape, sumValues);
    }

    @Override
    public IDoubleMatrix minus(IDoubleMatrix other) {
        return this.plus(other.times(-1));
    }

    @Override
    public IDoubleMatrix minus(double scalar) {
        return this.plus(-scalar);
    }

    protected static double[][] copyValues(double[][] values) {
        double[][] copy = new double[values.length][];
        for (int i = 0; i < values.length; i++) {
            assert values[i].length == values[0].length;
            copy[i] = new double[values[i].length];
            for (int j = 0; j < values[i].length; j++) copy[i][j] = values[i][j];
        }
        return copy;
    }

    protected static double[] copyVectorValues(double[] values) {
        double[] copy = new double[values.length];
        for (int i = 0; i < values.length; i++) copy[i] = values[i];
        return copy;
    }

    protected void getAssert(int row, int column) {
        assert this.shape != null;
        this.shape.assertInShape(row, column);
    }

    @Override
    public double normOne() {
        double result = 0;
        double columnSum;
        for (int i = 0; i < this.shape.columns; i++) {
            columnSum = 0;
            for (int j = 0; j < this.shape.rows; j++) {
                columnSum += abs(this.get(j, i));
            }
            if (columnSum > result) result = columnSum;
        }
        return result;
    }

    @Override
    public double normInfinity() {
        double result = 0;
        double rowSum;
        for (int i = 0; i < this.shape.rows; i++) {
            rowSum = 0;
            for (int j = 0; j < this.shape.columns; j++) {
                rowSum += abs(this.get(i, j));
            }
            if (rowSum > result) result = rowSum;
        }
        return result;
    }

    @Override
    public double frobeniusNorm() {
        double result = 0;
        double a;
        for (int i = 0; i < this.shape.rows; i++) {
            for (int j = 0; j < this.shape.columns; j++) {
                a = this.get(i, j);
                result += a * a;
            }
        }
        return sqrt(result);
    }

    @Override
    public String toString() {
        double[][] data = this.data();
        StringBuilder sb = new StringBuilder();
        sb.append("rows: ").append(this.shape.rows).append(", columns: ").append(this.shape.columns).append("\n");
        for (int i = 0; i < this.shape.rows; i++) {
            for (int j = 0; j < this.shape.columns - 1; j++) {
                sb.append(data[i][j]).append(" ");
            }
            sb.append(data[i][this.shape.columns - 1]).append("\n");
        }
        return sb.toString();
    }

    @Override
    public Shape shape() {
        return matrix(this.shape.rows, this.shape.columns);
    }

}

