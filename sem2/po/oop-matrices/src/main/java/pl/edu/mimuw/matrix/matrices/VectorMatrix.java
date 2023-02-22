package pl.edu.mimuw.matrix.matrices;

import pl.edu.mimuw.matrix.Shape;

//macierz, którą można opisać za pomocą pojedynczego wektora
public abstract class VectorMatrix extends DoubleMatrix {
    protected final double[] values;

    protected VectorMatrix(Shape shape, double[] values) {
        super(shape);
        this.values = DoubleMatrix.copyVectorValues(values);
    }

    public double[] getValues() {
        return copyVectorValues(this.values);
    }

    protected double[] valuesTimes(double scalar) {
        double[] productValues = new double[this.values.length];
        for (int i = 0; i < this.values.length; i++) {
            productValues[i] = this.values[i] * scalar;
        }
        return productValues;
    }

    protected double[] vectorPlus(VectorMatrix other) {
        assert this.getClass() == other.getClass();
        double[] sumValues = new double[this.values.length];
        double[] otherValues = other.getValues();
        assert this.values.length == otherValues.length;
        for (int i = 0; i < this.values.length; i++) {
            sumValues[i] = this.values[i] + otherValues[i];
        }
        return sumValues;
    }

}
