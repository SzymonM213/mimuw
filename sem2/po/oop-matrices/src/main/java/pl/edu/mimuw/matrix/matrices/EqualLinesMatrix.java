package pl.edu.mimuw.matrix.matrices;

import pl.edu.mimuw.matrix.Shape;

public abstract class EqualLinesMatrix extends VectorMatrix {

    protected EqualLinesMatrix(Shape shape, double[] values) {
        super(shape, values);
    }

    protected double[] linePlus(double scalar) {
        double[] sumValues = new double[this.values.length];
        for (int i = 0; i < this.values.length; i++) {
            sumValues[i] = this.values[i] + scalar;
        }
        return sumValues;
    }

}
