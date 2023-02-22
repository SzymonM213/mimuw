package pl.edu.mimuw.matrix.matrices;

import pl.edu.mimuw.matrix.IDoubleMatrix;
import pl.edu.mimuw.matrix.Shape;

public class JustVector extends VectorMatrix {

    public JustVector(Shape shape, double[] values) {
        super(shape, values);
        assert shape.columns == 1;
    }

    @Override
    public IDoubleMatrix times(double scalar) {
        return new JustVector(this.shape, this.valuesTimes(scalar));
    }

    @Override
    public double get(int row, int column) {
        this.getAssert(row, column);
        return this.values[row];
    }

    @Override
    public double[][] data() {
        double[][] tmp = new double[this.values.length][1];
        for (int i = 0; i < tmp.length; i++) tmp[i][0] = this.values[i];
        return tmp;
    }

    @Override
    public IDoubleMatrix copy() {
        return new JustVector(this.shape, this.values);
    }
}
