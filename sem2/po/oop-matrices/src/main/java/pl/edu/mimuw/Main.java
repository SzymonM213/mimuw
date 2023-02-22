package pl.edu.mimuw;

import pl.edu.mimuw.matrix.IDoubleMatrix;
import pl.edu.mimuw.matrix.Shape;

import static pl.edu.mimuw.matrix.DoubleMatrixFactory.*;
import static pl.edu.mimuw.matrix.MatrixCellValue.cell;
import static pl.edu.mimuw.matrix.Shape.matrix;

public class Main {

  public static void main(String[] args) {
    double[] values = new double[]{2, 1, 3, 7, 6, 9, 2, 1, 1, 5};
    Shape shape = matrix(values.length, values.length);
    double[][] twoDimValues = new double[values.length][values.length];
    for(int i =0;i<values.length;i++) {
      for(int j =0; j<values.length; j++) {
        twoDimValues[i][j] = j+i*69;
      }
    }

    IDoubleMatrix diagonal = diagonal(values);
    IDoubleMatrix antiDiagonal = antiDiagonal(values);
    IDoubleMatrix column = column(shape, values);
    IDoubleMatrix row = row(shape, values);
    IDoubleMatrix full = full(twoDimValues);
    IDoubleMatrix constant = constant(shape, 6);
    IDoubleMatrix identity = identity(shape.columns);
    IDoubleMatrix vector = vector(values);
    IDoubleMatrix sparse = sparse(shape,
            cell(2, 3, 2137),
            cell(6, 7, 69),
            cell(5, 3, 420),
            cell(1, 9, 3));


    System.out.println("diagonal matrix:\n" + diagonal);
    System.out.println("NormOne: " + diagonal.normOne() + " NormInfinity: " + diagonal.normInfinity() +
            " FrobeniusNorm: " + diagonal.frobeniusNorm());
    System.out.println("anti diagonal matrix:\n" + antiDiagonal + "\n");
    System.out.println("NormOne: " + antiDiagonal.normOne() + " NormInfinity: " + antiDiagonal.normInfinity() +
            " FrobeniusNorm: " + antiDiagonal.frobeniusNorm() + "\n");
    System.out.println("column matrix:\n" + column);
    System.out.println("NormOne: " + column.normOne() + " NormInfinity: " + column.normInfinity() +
            " FrobeniusNorm: " + column.frobeniusNorm() + "\n");
    System.out.println("row matrix:\n" + row);
    System.out.println("NormOne: " + row.normOne() + " NormInfinity: " + row.normInfinity() +
            " FrobeniusNorm: " + row.frobeniusNorm() + "\n");
    System.out.println("full matrix:\n" + full);
    System.out.println("NormOne: " + full.normOne() + " NormInfinity: " + full.normInfinity() +
            " FrobeniusNorm: " + full.frobeniusNorm() + "\n");
    System.out.println("constant matrix:\n" + constant);
    System.out.println("NormOne: " + column.normOne() + " NormInfinity: " + column.normInfinity() +
            " FrobeniusNorm: " + column.frobeniusNorm() + "\n");
    System.out.println("identity matrix:\n" + identity);
    System.out.println("NormOne: " + identity.normOne() + " NormInfinity: " + identity.normInfinity() +
            " FrobeniusNorm: " + identity.frobeniusNorm() + "\n");
    System.out.println("vector:\n" + vector);
    System.out.println("NormOne: " + vector.normOne() + " NormInfinity: " + vector.normInfinity() +
            " FrobeniusNorm: " + vector.frobeniusNorm() + "\n");
    System.out.println("sparse matrix:\n" + sparse);
    System.out.println("NormOne: " + sparse.normOne() + " NormInfinity: " + sparse.normInfinity() +
            " FrobeniusNorm: " + sparse.frobeniusNorm() + "\n");
  }
}
