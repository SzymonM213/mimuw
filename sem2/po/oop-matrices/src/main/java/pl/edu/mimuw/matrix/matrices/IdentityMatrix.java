package pl.edu.mimuw.matrix.matrices;

import pl.edu.mimuw.matrix.IDoubleMatrix;
import pl.edu.mimuw.matrix.Shape;

import static java.lang.Math.sqrt;

public class IdentityMatrix extends DoubleMatrix {

    public IdentityMatrix(Shape shape) {
        super(shape);
    }

    @Override
    public IDoubleMatrix times(IDoubleMatrix other) {
        assert this.shape.columns == other.shape().columns;
        return this.copy();
    }

    private IDoubleMatrix diagonalIdentity() {
        double[] oneValue = new double[this.shape.columns];
        for (int i = 0; i < this.shape.columns; i++) oneValue[i] = 1;
        return new NormalDiagonalMatrix(this.shape, oneValue);
    }

    @Override
    public IDoubleMatrix times(double scalar) {
        double[] scalarValues = new double[this.shape.columns];
        for (int i = 0; i < this.shape.columns; i++) scalarValues[i] = scalar;
        return new NormalDiagonalMatrix(this.shape, scalarValues);
    }

    @Override
    public IDoubleMatrix plus(IDoubleMatrix other) {
        return other.plus(this.diagonalIdentity());
    }

    public double get(int row, int column) {
        this.getAssert(row, column);
        if (row == column) return 1;
        return 0;
    }

    @Override
    public double[][] data() {
        double[] ones = new double[this.shape.columns];
        for (int i = 0; i < this.shape.columns; i++) ones[i] = 1;
        IDoubleMatrix tmp = new NormalDiagonalMatrix(shape, ones);
        return tmp.data();
    }

    @Override
    public double normOne() {
        return 1;
    }

    @Override
    public double normInfinity() {
        return 1;
    }

    @Override
    public double frobeniusNorm() {
        return sqrt(this.shape.columns);
    }

    @Override
    public String toString() {
        return this.diagonalIdentity().toString();
    }

    @Override
    public IDoubleMatrix copy() {
        return new IdentityMatrix(this.shape);
    }

}
