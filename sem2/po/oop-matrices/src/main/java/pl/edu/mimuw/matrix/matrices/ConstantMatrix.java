package pl.edu.mimuw.matrix.matrices;

import pl.edu.mimuw.matrix.IDoubleMatrix;
import pl.edu.mimuw.matrix.Shape;

import static pl.edu.mimuw.matrix.Shape.matrix;

import static java.lang.Math.sqrt;

public class ConstantMatrix extends DoubleMatrix {
    private final double value;

    public ConstantMatrix(Shape shape, double value) {
        super(shape);
        this.value = value;
    }

    @Override
    public IDoubleMatrix times(IDoubleMatrix other) {
        if (this.value == 0) return this.copy();
        else return super.times(other);
    }

    @Override
    public IDoubleMatrix times(double scalar) {
        return new ConstantMatrix(this.shape, this.value * scalar);
    }

    @Override
    public IDoubleMatrix plus(double scalar) {
        return new ConstantMatrix(this.shape, this.value + scalar);
    }

    @Override
    public double get(int row, int column) {
        super.getAssert(row, column);
        return this.value;
    }

    @Override
    public double[][] data() {
        double[][] values = new double[this.shape.rows][this.shape.columns];
        for (var column : values) {
            for (int i = 0; i < column.length; i++) column[i] = this.value;
        }
        return values;
    }

    @Override
    public double normOne() {
        return this.value * this.shape.rows;
    }

    @Override
    public double normInfinity() {
        return this.value * this.shape.columns;
    }

    @Override
    public double frobeniusNorm() {
        return sqrt(this.value * this.value * this.shape.columns * this.shape.rows);
    }

    @Override
    public String toString() {
        if (this.shape.columns < 3) return super.toString();
        StringBuilder sb = new StringBuilder();
        sb.append("rows: ").append(this.shape.rows).append(", columns: ").append(this.shape.columns).append("\n");
        for (int i = 0; i < this.shape.rows; i++) {
            sb.append(this.value).append(" ... ").append(this.value).append("\n");
        }
        return sb.toString();
    }

    @Override
    public Shape shape() {
        return matrix(this.shape.rows, this.shape.columns);
    }

    @Override
    public IDoubleMatrix copy() {
        return new ConstantMatrix(this.shape, this.value);
    }

}
