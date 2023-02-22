package pl.edu.mimuw.matrix.matrices;

import pl.edu.mimuw.matrix.IDoubleMatrix;
import pl.edu.mimuw.matrix.MatrixCellValue;
import pl.edu.mimuw.matrix.Shape;

import java.util.*;

import static java.lang.Math.abs;
import static java.lang.Math.sqrt;

public class SparseMatrix extends DoubleMatrix {
    private final List<List<MatrixCellValue>> values;

    public SparseMatrix(Shape shape, MatrixCellValue[] values) {
        super(shape);
        this.values = new ArrayList<>();
        this.values.add(new ArrayList<>());
        for (var v : values) {
            super.getAssert(v.row, v.column);
            if (this.values.get(this.values.size() - 1).size() != 0 &&
                    this.values.get(this.values.size() - 1).get(0).row != v.row) {
                this.values.add(new ArrayList<>());
            }
            this.values.get(this.values.size() - 1).add(v);
        }
        this.sortValues();
    }

    public SparseMatrix(Shape shape, List<List<MatrixCellValue>> values) {
        super(shape);
        this.values = new ArrayList<>();
        for (int row = 0; row < values.size(); row++) {
            this.values.add(new ArrayList<>());
            this.values.get(row).addAll(values.get(row));
        }
        this.sortValues();
    }

    private void sortValues() {
        for (var v : this.values) {
            Collections.sort(v, MatrixCellValue::compareTo);
        }
    }

    @Override
    public IDoubleMatrix times(double scalar) {
        List<List<MatrixCellValue>> productValues = new ArrayList<>();
        for (int row = 0; row < this.values.size(); row++) {
            productValues.add(new ArrayList<>());
            for (var v : this.values.get(row)) {
                productValues.get(row).add(new MatrixCellValue(v.row, v.column, v.value * scalar));
            }
        }
        return new SparseMatrix(this.shape, productValues);
    }

    public List<MatrixCellValue> getNonZeroValues() {
        List<MatrixCellValue> copy = new ArrayList<>();
        for (var v : this.values) {
            copy.addAll(v);
        }
        return copy;
    }

    private IDoubleMatrix sparseTimes(SparseMatrix other) {
        assert this.shape.columns == other.shape().rows;
        List<List<MatrixCellValue>> productValues = new ArrayList<>();
        productValues.add(new ArrayList<>());
        Map<Integer, List<MatrixCellValue>> otherColumns = new HashMap<>();
        for (var v : other.getNonZeroValues()) {
            if (otherColumns.get(v.column) == null) {
                List<MatrixCellValue> tmp = new ArrayList<>();
                tmp.add(v);
                otherColumns.put(v.column, tmp);
            } else {
                otherColumns.get(v.column).add(v);
            }
        }
        int i;
        double value;
        for (int row = 0; row < this.values.size(); row++) {
            for (int column = 0; column < other.shape().columns; column++) {
                i = 0;
                //Sprawdzenie, czy jest sens w ogóle coś mnożyć w danej kolumnie
                if (otherColumns.get(column) != null) {
                    value = 0;
                    int j = 0;
                    while (i < this.values.get(row).size() && j < otherColumns.get(column).size()) {
                        if (this.values.get(row).get(i).column < otherColumns.get(column).get(j).row) i++;
                        else if (this.values.get(row).get(i).column > otherColumns.get(column).get(j).row) j++;
                        else value += this.values.get(row).get(i).value * otherColumns.get(column).get(j++).value;
                    }
                    if (value != 0) {
                        if (productValues.get(productValues.size() - 1).size() != 0 && productValues.get(productValues.size() - 1).get(0).row != row) {
                            productValues.add(new ArrayList<>());
                        }
                        productValues.get(productValues.size() - 1).add(
                                new MatrixCellValue(this.values.get(row).get(0).row, column, value));
                    }
                }
            }
        }
        return new SparseMatrix(Shape.matrix(this.shape.rows, other.shape().columns), productValues);
    }

    @Override
    public IDoubleMatrix times(IDoubleMatrix other) {
        if (this.getClass() == other.getClass()) {
            return this.sparseTimes((SparseMatrix) other);
        } else {
            return super.times(other);
        }
    }

    public List<MatrixCellValue> getRow(int i) {
        List<MatrixCellValue> copy = new ArrayList<>();
        for (var v : this.values) {
            if (i == v.get(0).row) {
                copy.addAll(v);
                break;
            }
        }
        return copy;
    }

    private IDoubleMatrix sparsePlus(SparseMatrix other) {
        assert this.shape.columns == other.shape().columns;
        assert this.shape.rows == other.shape().rows;
        List<MatrixCellValue> otherRow;
        List<MatrixCellValue> sumValues = new ArrayList<>();
        for (int row = 0; row < this.values.size(); row++) {
            int i = 0;
            int j = 0;
            otherRow = other.getRow(this.values.get(row).get(0).row);
            while (i < this.values.get(row).size() && j < otherRow.size()) {
                if (this.values.get(row).get(i).column < otherRow.get(j).column) {
                    sumValues.add(new MatrixCellValue(this.values.get(row).get(i++)));
                } else if (this.values.get(row).get(i).column > otherRow.get(j).column) {
                    sumValues.add(new MatrixCellValue(otherRow.get(j++)));
                } else
                    sumValues.add(new MatrixCellValue(this.values.get(row).get(0).row, this.values.get(row).get(i).column,
                            this.values.get(row).get(i++).value + otherRow.get(j++).value));
            }
            while (i < this.values.get(row).size())
                sumValues.add(new MatrixCellValue(this.values.get(row).get(i++)));
            while (j < otherRow.size())
                sumValues.add(new MatrixCellValue(otherRow.get(j++)));
        }
        return new SparseMatrix(this.shape, sumValues.toArray(new MatrixCellValue[sumValues.size()]));
    }

    @Override
    public IDoubleMatrix plus(IDoubleMatrix other) {
        if (this.getClass() == other.getClass()) {
            return this.sparsePlus((SparseMatrix) other);
        } else {
            return super.plus(other);
        }
    }

    @Override
    public double get(int row, int column) {
        super.getAssert(row, column);
        for (var v1 : this.values) {
            if (v1.get(0).row == row) {
                for (var v2 : v1) {
                    if (v2.column == column) return v2.value;
                }
            }
        }
        return 0;
    }

    @Override
    public double[][] data() {
        double[][] twoDimValues = new ConstantMatrix(this.shape, 0).data();
        for (var row : this.values) {
            for (var v : row) {
                twoDimValues[v.row][v.column] = v.value;
            }
        }
        return twoDimValues;
    }

    @Override
    public double normOne() {
        double[] columnSums = new double[this.shape.columns];
        for (int i = 0; i < this.shape.columns; i++) columnSums[i] = 0;
        for (var row : this.values) {
            for (var v : row) columnSums[v.column] += abs(v.value);
        }
        double result = 0;
        for (var v : columnSums) if (v > result) result = v;
        return result;
    }

    @Override
    public double normInfinity() {
        double result = 0;
        double sum;
        for (var row : this.values) {
            sum = 0;
            for (var v : row) {
                sum += abs(v.value);
            }
            if (sum > result) result = sum;
        }
        return result;
    }

    @Override
    public double frobeniusNorm() {
        double result = 0;
        for (var row : this.values) {
            for (var v : row) result += v.value * v.value;
        }
        return sqrt(result);
    }

    @Override
    public IDoubleMatrix copy() {
        return new SparseMatrix(this.shape, this.values);
    }
}
