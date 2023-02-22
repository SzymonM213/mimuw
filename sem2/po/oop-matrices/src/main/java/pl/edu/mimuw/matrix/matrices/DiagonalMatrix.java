package pl.edu.mimuw.matrix.matrices;

import pl.edu.mimuw.matrix.Shape;

import static java.lang.Math.abs;
import static java.lang.Math.sqrt;

public abstract class DiagonalMatrix extends VectorMatrix {

    protected DiagonalMatrix(Shape shape, double[] values) {
        super(shape, values);
        assert shape.columns == shape.rows;
        assert shape.columns == values.length;
    }

    private double normOneInf() {
        double result = 0;
        for (var v : this.values) {
            if (abs(v) > result) result = abs(v);
        }
        return result;
    }

    @Override
    public double normOne() {
        return this.normOneInf();
    }

    @Override
    public double normInfinity() {
        return this.normOneInf();
    }

    @Override
    public double frobeniusNorm() {
        double result = 0;
        for (var v : this.values) {
            result += v * v;
        }
        return sqrt(result);
    }

}
