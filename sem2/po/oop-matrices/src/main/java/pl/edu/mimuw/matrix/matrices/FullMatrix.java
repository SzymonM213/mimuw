package pl.edu.mimuw.matrix.matrices;

import pl.edu.mimuw.matrix.IDoubleMatrix;
import pl.edu.mimuw.matrix.Shape;

public class FullMatrix extends DoubleMatrix {
    private final double[][] values;

    public FullMatrix(Shape shape, double[][] values) {
        super(shape);
        this.values = DoubleMatrix.copyValues(values);
    }

    @Override
    public IDoubleMatrix times(double scalar) {
        double[][] sumValues = new double[this.shape.rows][this.shape.columns];
        for (int i = 0; i < this.shape.rows; i++) {
            for (int j = 0; j < this.shape.columns; j++) {
                sumValues[i][j] = this.values[i][j] * scalar;
            }
        }
        return new FullMatrix(this.shape, sumValues);
    }

    @Override
    public IDoubleMatrix minus(IDoubleMatrix other) {
        return plus(other.times(-1));
    }

    @Override
    public IDoubleMatrix minus(double scalar) {
        return this.plus(-scalar);
    }

    @Override
    public double get(int row, int column) {
        super.getAssert(row, column);
        return this.values[row][column];
    }

    @Override
    public double[][] data() {
        return copyValues(this.values);
    }

    @Override
    public IDoubleMatrix copy() {
        return new FullMatrix(this.shape, this.values);
    }
}
