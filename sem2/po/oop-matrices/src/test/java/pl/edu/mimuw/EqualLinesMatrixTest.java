package pl.edu.mimuw;

import org.junit.jupiter.api.Test;
import pl.edu.mimuw.matrix.IDoubleMatrix;
import pl.edu.mimuw.matrix.matrices.ColumnMatrix;
import pl.edu.mimuw.matrix.matrices.ConstantMatrix;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static pl.edu.mimuw.matrix.DoubleMatrixFactory.column;
import static pl.edu.mimuw.matrix.DoubleMatrixFactory.row;
import static pl.edu.mimuw.matrix.Shape.matrix;


class EqualLinesMatrixTest {

    double[] values = new double[]{1, 2, 3};
    double[] values2 = new double[]{4, 5, 6};
    double[] values3 = new double[]{2, 1, 3, 7};

    @Test
    void testPathologicalArgumentsColumn() {
        assertThrows(AssertionError.class, () -> column(matrix(69, 420), null));

        assertThrows(AssertionError.class, () -> column(null, values));

        assertThrows(AssertionError.class, () -> column(matrix(69, 420), values));
    }

    @Test
    void testPathologicalArgumentsRow() {
        assertThrows(AssertionError.class, () -> row(matrix(69, 420), null));

        assertThrows(AssertionError.class, () -> row(null, values));

        assertThrows(AssertionError.class, () -> row(matrix(69, 420), values));
    }

    @Test
    void testPlusColumn() {
        IDoubleMatrix m1 = column(matrix(3, 3), values);
        IDoubleMatrix m2 = column(matrix(3, 3), values2);
        IDoubleMatrix m3 = column(matrix(3, 2), values2);

        assertEquals(9, m1.plus(m2).get(2, 2));
        assertEquals(5, m1.plus(m2).get(0, 0));
        assertEquals(7, m1.plus(5).get(1, 1));
        assertThrows(AssertionError.class, () -> m1.plus(m3));
    }

    @Test
    void testPlusRow() {
        IDoubleMatrix m1 = row(matrix(3, 3), values);
        IDoubleMatrix m2 = row(matrix(3, 3), values2);
        IDoubleMatrix m3 = row(matrix(2, 3), values2);

        assertEquals(9, m1.plus(m2).get(2, 2));
        assertEquals(5, m1.plus(m2).get(0, 0));
        assertEquals(7, m1.plus(5).get(1, 1));
        assertThrows(AssertionError.class, () -> m3.plus(m1));
    }

    @Test
    void testPlusMixed() {
        IDoubleMatrix m1 = row(matrix(3, 3), values);
        IDoubleMatrix m2 = column(matrix(3, 3), values2);
        IDoubleMatrix m3 = row(matrix(2, 3), values2);

        assertEquals(5, m1.plus(m2).get(0, 0));
        assertEquals(6, m1.plus(m2).get(0, 1));
        assertEquals(7, m1.plus(m2).get(0, 2));
        assertThrows(AssertionError.class, () -> m3.plus(m1));
    }

    @Test
    void testTimesColumn() {
        IDoubleMatrix m1 = column(matrix(3, 4), values);
        IDoubleMatrix m2 = column(matrix(4, 4), values3);
        IDoubleMatrix m3 = column(matrix(3, 2), values2);

        assertEquals(39, m1.times(m2).get(2, 2));
        assertEquals(13, m1.times(m2).get(0, 0));
        assertEquals(10, m1.times(5).get(1, 1));
        assertThrows(AssertionError.class, () -> m1.times(m3));
    }

    @Test
    void testTimesRow() {
        IDoubleMatrix m1 = row(matrix(4, 3), values);
        IDoubleMatrix m2 = row(matrix(3, 4), values3);
        IDoubleMatrix m3 = row(matrix(2, 3), values2);

        assertEquals(18, m1.times(m2).get(2, 2));
        assertEquals(12, m1.times(m2).get(0, 0));
        assertEquals(10, m1.times(5).get(1, 1));
        assertThrows(AssertionError.class, () -> m1.times(m3));
    }

    @Test
    void testTimesMixed() {
        IDoubleMatrix m1 = column(matrix(3, 3), values);
        IDoubleMatrix m2 = row(matrix(3, 4), values3);
        IDoubleMatrix m3 = column(matrix(3, 3), values2);

        assertEquals(27, m1.times(m2).get(2, 2));
        assertEquals(6, m1.times(m2).get(0, 0));
        assertEquals(10, m1.times(5).get(1, 1));
        assertThrows(AssertionError.class, () -> m2.times(m3));
    }

    @Test
    void testNormColumn() {
        IDoubleMatrix m1 = column(matrix(3, 3), values);

        assertEquals(6, m1.normOne());
        assertEquals(9, m1.normInfinity());
        assertEquals(6.48074069840786, m1.frobeniusNorm());
    }

    @Test
    void testNormRow() {
        IDoubleMatrix m1 = row(matrix(3, 3), values);

        assertEquals(9, m1.normOne());
        assertEquals(6, m1.normInfinity());
        assertEquals(6.48074069840786, m1.frobeniusNorm());
    }

}