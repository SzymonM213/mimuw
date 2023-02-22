package pl.edu.mimuw.matrix.matrices;

import pl.edu.mimuw.matrix.IDoubleMatrix;
import pl.edu.mimuw.matrix.Shape;

public class AntiDiagonalMatrix extends DiagonalMatrix {

    public AntiDiagonalMatrix(Shape shape, double[] values) {
        super(shape, values);
    }

    @Override
    public IDoubleMatrix times(double scalar) {
        return new AntiDiagonalMatrix(this.shape, super.valuesTimes(scalar));
    }

    @Override
    public IDoubleMatrix plus(IDoubleMatrix other) {
        if(this.getClass() == other.getClass()) {
            return new AntiDiagonalMatrix(this.shape, this.vectorPlus((VectorMatrix) other));
        }
        else {
            return super.plus(other);
        }
    }

    @Override
    public double get(int row, int column) {
        super.getAssert(row, column);
        if(row + column == this.shape.columns-1) return this.values[row];
        else return 0;
    }

    @Override
    public double[][] data() {
        double[][] twoDimValues = new double[this.shape.rows][this.shape.columns];
        for(int i = 0; i<this.shape.rows;i++) {
            for(int j = 0;j<this.shape.columns;j++) {
                if(i==this.shape.columns-1-j) twoDimValues[i][j] = values[i];
                else twoDimValues[i][j] = 0;
            }
        }
        return twoDimValues;
    }

    @Override
    public String toString() {
        if(this.shape.columns < 4) return super.toString();
        StringBuilder sb = new StringBuilder();
        sb.append("rows: ").append(this.shape.rows).append(", columns: ").append(this.shape.columns).append("\n");
        for(int i=0;i<this.shape.columns;i++) {
            for(int j=0;j+i<this.shape.columns-1;j++) {
                sb.append(this.get(i, j)).append(" ");
                if(j==0 && i<this.shape.columns-3) {
                    sb.append("... ");
                    j=this.shape.columns-3-i;
                }
            }
            sb.append(this.get(i, this.shape.columns-1-i)).append(" ");
            for(int j=this.shape.columns-i;j<this.shape.columns;j++) {
                sb.append(this.get(i, j)).append(" ");
                if(j==this.shape.columns-i && i>2) {
                    sb.append("... ");
                    j=this.shape.columns-2;
                }
            }
            sb.append("\n");
        }
        return sb.toString();
    }

    @Override
    public IDoubleMatrix copy() {
        return new AntiDiagonalMatrix(this.shape, this.values);
    }
}
